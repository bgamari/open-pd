#include <mchck.h>

#include "power-meter.desc.h"

static bool amp_on = false;

static accum wavelength = 488;

struct pair {
	accum x, y;
};

// in milliamps per watt
struct pair sensitivity_lut[] = {
	{1, 1},
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
// Minimum tolerable voltage before increasing gain
static accum autoscale_min_thresh = 0.1;
// Maximum tolerable voltage before decreasing gain
static accum autoscale_max_thresh = 3.1;
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
	float v = adc_as_voltage(val);
	accum sensitivity = interpolate(sensitivity_lut, wavelength);
	accum current = v * stage_gain[stage] * range_gain[active_range];
	accum power = current * sensitivity;

	unsigned long mv = 1000. * v;
        printf("power %d: raw=%lu\r\n", stage, mv);

	if (autoscale && v < autoscale_min_thresh && active_range != RANGE4) {
		set_range(active_range + 1);
		printf("# moving gain to up range %d\r\n", active_range);
	} else if (autoscale && v > autoscale_max_thresh && active_range != RANGE1) {
		set_range(active_range - 1);
		printf("# moving gain to down range %d\r\n", active_range);
	} else if (v < autoscale_min_thresh && stage == STAGE1) {
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
