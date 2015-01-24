#include <mchck.h>

#include "power-meter.desc.h"

static bool amp_on = false;

static accum wavelength = 488;

struct pair {
	accum x, y;
};

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
#define PD2 ADC_PTB2

// range switches
#define SEL_A GPIO_PTC1
#define SEL_B GPIO_PTC2
#define SEL_C GPIO_PTC3

#define LED1 GPIO_PTD1
#define LED2 GPIO_PTD3

#define PD_EN GPIO_PTA19

enum range {
	RANGE1 = 0b101,  // lowest gain
	RANGE2 = 0b001,
	RANGE3 = 0b010,
	RANGE4 = 0x000,  // highest gain
};

void set_range(enum range rng) {
	if (!amp_on) return;
	gpio_write(SEL_B, rng & 2);
	gpio_write(SEL_C, rng & 4);
	gpio_write(SEL_A, rng & 1);
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
	}
	gpio_write(LED1, on);
	gpio_write(LED2, on);
}

void sample_pd_done(uint16_t val, int error, void *cbdata) {
	unsigned accum v = adc_as_voltage(val);
	accum sensitivity = interpolate(sensitivity_lut, wavelength);
	unsigned long mv = 1000. * v;
        printf("power: raw=%lu\r\n", mv);
}	

int sample_pd() {
	return adc_sample_start(ADC_PTB1, sample_pd_done, NULL);
}

static struct cdc_ctx cdc;

static void new_data(uint8_t *data, size_t len)
{
	switch (data[0]) {
	case '1':
		set_range(RANGE1);
		printf("set range 1\r\n");
		break;
	case '2':
		set_range(RANGE2);
		printf("set range 2\r\n");
		break;
	case '3':
		set_range(RANGE3);
		printf("set range 3\r\n");
		break;
	case '4':
		set_range(RANGE4);
		printf("set range 4\r\n");
		break;
	}
	sample_pd();
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

	gpio_dir(SEL_A, GPIO_OUTPUT);
	gpio_dir(SEL_B, GPIO_OUTPUT);
	gpio_dir(SEL_C, GPIO_OUTPUT);
	pin_mode(SEL_A, PIN_MODE_MUX_GPIO);
	pin_mode(SEL_B, PIN_MODE_MUX_GPIO);
	pin_mode(SEL_C, PIN_MODE_MUX_GPIO);

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
