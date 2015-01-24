#include <mchck.h>

#include "power-meter.desc.h"

static bool amp_on = false;

static accum wavelength = 488;

struct pair {
	accum x, y;
};

// wavelength in nanometers
// sensitivity in milliamps per watt
struct pair sensitivity_lut[] = {
	{200,    117},
	{210,    125},
	{220,    128},
	{230,    133},
	{240,    137},
	{250,    130},
	{260,    118},
	{270,    100},
	{280,    102},
	{290,    116},
	{300,    130},
	{310,    137},
	{320,    140},
	{330,    148},
	{340,    151},
	{350,    151},
	{360,    151},
	{370,    152},
	{380,    162},
	{390,    177},
	{400,    187},
	{420,    205},
	{440,    221},
	{460,    234},
	{480,    246},
	{500,    261},
	{520,    274},
	{540,    285},
	{560,    297},
	{580,    310},
	{600,    321},
	{620,    333},
	{640,    346},
	{660,    356},
	{680,    368},
	{700,    377},
	{720,    389},
	{740,    401},
	{760,    412},
	{780,    425},
	{800,    434},
	{820,    444},
	{840,    456},
	{860,    467},
	{880,    478},
	{900,    491},
	{920,    500},
	{940,    511},
	{960,    522},
	{980,    520},
	{1000,   505},
	{1020,   464},
	{1040,   391},
	{1060,   287},
	{1080,   206},
	{1100,   142},
	// sentinal
	{-1, -1}
};

float interpolate(struct pair* samples, float x) {
	while (1) {
		struct pair* next = samples+1;
		if (next->x == -1)
			break;
		if (next->x > x)
			return samples->y + (next->y - samples->y) / (next->x - samples->x) * (x - samples->x);
		samples++;
	}
	return samples->y;
}

#define PD1 ADC_PTB1
#define PD2 ADC_PTB0

enum gain_stage {
	STAGE1, STAGE2
};

// gain of voltage stages
float stage_gain[2] = {1, 100};

// range switches
#define SEL_A GPIO_PTC1
#define SEL_B GPIO_PTC2
#define SEL_C GPIO_PTC3

#define LED1 GPIO_PTD1
#define LED2 GPIO_PTD3

#define PD_EN GPIO_PTA19

enum range {
	RANGE1,  // lowest gain
	RANGE2,
	RANGE3,
	RANGE4,  // highest gain
};

uint8_t range_muxes[4] = {
	0b101,     // RANGE1
	0b001,     // RANGE2
	0b010,     // RANGE3
	0b000,     // RANGE4
};

// In volts per amp
float range_gain[4] = {
	1,
	150,
	33e3,
	4.7e6,
};

static bool autoscale = false;
// Minimum tolerable voltage before increasing gain in microvolts
static uint32_t autoscale_min_thresh = 0.1 * 1e6;
// Maximum tolerable voltage before decreasing gain in microvolts
static uint32_t autoscale_max_thresh = 3.1 * 1e6;
static enum range active_range;

void set_range(enum range rng) {
	active_range = rng;
	if (!amp_on) return;
	uint8_t mux = range_muxes[rng];
	gpio_write(SEL_B, (mux & 2) != 0);
	gpio_write(SEL_C, (mux & 4) != 0);
	gpio_write(SEL_A, (mux & 1) != 0);
}

void set_power(bool on) {
	amp_on = on;
	if (!on) {
		gpio_write(SEL_A, 0);
		gpio_write(SEL_B, 0);
		gpio_write(SEL_C, 0);
		gpio_write(PD_EN, 0);
	} else {
		gpio_write(PD_EN, 1);
		set_range(active_range);
	}
	gpio_write(LED1, on);
	gpio_write(LED2, on);
}

int sample_pd(enum gain_stage stage);

void sample_pd_done(uint16_t val, int error, void *cbdata) {
	enum gain_stage stage = (enum gain_stage) cbdata;
	// ADC voltage in microvolts
	uint32_t microvolts = 1e6 * adc_as_voltage(val);
	// photodiode current in microamps
	float microamps = 1. * microvolts / stage_gain[stage] / range_gain[active_range];
	float sensitivity = interpolate(sensitivity_lut, wavelength);
	// power in microwatts
	float power = 1000. * microamps / sensitivity;

	char *unit;
	uint32_t real_power;
	int exp;
	if (power > 1e8) {
		unit = "";
		exp = 0;
		real_power = power / 1e6;
	} else if (power > 1e5) {
		unit = "milli";
		exp = -3;
		real_power = power / 1e3;
	} else if (power > 1e2) {
		unit = "micro";
		exp = -6;
		real_power = power;
	} else {
		unit = "nano";
		exp = -9;
		real_power = power * 1e3;
	}
	//printf("%d %luE%d  # %lu %swatts\r\n", stage, real_power, exp, real_power, unit);

        printf("%d\t%d\t%lu\r\n", stage, active_range, microvolts);

	if (stage == STAGE2) {
		return;
	} else if (autoscale && microvolts < autoscale_min_thresh && active_range != RANGE4) {
		set_range(active_range + 1);
		printf("# moving gain to up range %d\r\n", active_range);
	} else if (autoscale && microvolts > autoscale_max_thresh && active_range != RANGE1) {
		set_range(active_range - 1);
		printf("# moving gain to down range %d\r\n", active_range);
	} else if (microvolts < autoscale_min_thresh && stage == STAGE1) {
		sample_pd(STAGE2);
	}
}	

int sample_pd(enum gain_stage stage) {
	adc_sample_prepare(ADC_MODE_SAMPLE_LONG | ADC_MODE_POWER_NORMAL | ADC_MODE_AVG_32);
	return adc_sample_start(stage == STAGE1 ? PD1 : PD2, sample_pd_done, (void*) stage);
}

static struct cdc_ctx cdc;

static void new_data(uint8_t *data, size_t len)
{
	switch (data[0]) {
	case '1':
	case '2':
	case '3':
	case '4':
		{
			enum range rng = data[0] - '0';
			set_range(rng);
			printf("# set range %d\r\n", rng);
			break;
		}
	case 'a':
		autoscale = false;
		break;
	case 'A':
		autoscale = true;
		break;
	}
	sample_pd(STAGE1);
        cdc_read_more(&cdc);
}

void
init_vcdc(int enable)
{
        if (enable) {
                cdc_init(new_data, NULL, &cdc);
		cdc_set_stdout(&cdc);
        }
}

int main() {
        pin_mode(PD_EN, PIN_MODE_MUX_GPIO);
	gpio_dir(PD_EN, GPIO_OUTPUT);

	pin_mode(SEL_A, PIN_MODE_MUX_GPIO);
	pin_mode(SEL_B, PIN_MODE_MUX_GPIO);
	pin_mode(SEL_C, PIN_MODE_MUX_GPIO);
	gpio_dir(SEL_A, GPIO_OUTPUT);
	gpio_dir(SEL_B, GPIO_OUTPUT);
	gpio_dir(SEL_C, GPIO_OUTPUT);

	gpio_dir(LED1, GPIO_OUTPUT);
	gpio_dir(LED2, GPIO_OUTPUT);
        pin_mode(LED1, PIN_MODE_DRIVE_HIGH);
        pin_mode(LED2, PIN_MODE_DRIVE_HIGH);

        pin_mode(PIN_PTB1, PIN_MODE_MUX_ANALOG);
        pin_mode(PIN_PTB2, PIN_MODE_MUX_ANALOG);

	adc_init();
	set_range(RANGE1);
	set_power(true);

        usb_init(&cdc_device);
        sys_yield_for_frogs();
}
