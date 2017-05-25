#include <mchck.h>

#include "power-meter.desc.h"

static bool amp_on = false;

// Verbose
static bool verbose = false;

static uint8_t oversample = 16;
static uint8_t oversample_counter;
static uint32_t oversample_accum;

struct pair {
        uint16_t x, y;
};

#define CONFIG_MAGIC 0xdead0ed0


struct config {
        uint32_t magic;
        uint16_t wavelength;      // in nanometers
        float range_gains[8];     // in volt/amp
        float range_offsets[4];   // in microamps

        // wavelength in nanometers
        // sensitivity in milliamps per watt
        struct pair sensitivity_lut[64];
};

static struct config *flash_config = (struct config*) 0x10000000;

static struct config active_config;

static const struct config default_config = {
        .magic = CONFIG_MAGIC,
        .wavelength = 488,
        .range_gains = {
                1,        1*11,
                150,      150*11,
                33e3,     33e3*11,
                4.7e6,    4.7e6*11
        },
        .sensitivity_lut = {
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
        },
};

float interpolate(struct pair* samples, float x) {
        for (int i=0; i<64; i++) {
                struct pair* next = samples+1;
                if (next->y == 0)
                        break;
                if (next->x > x)
                        return samples->y + (next->y - samples->y) / (next->x - samples->x) * (x - samples->x);
                samples++;
        }
        return samples->y;
}

// Gain stage ADC taps
#define STAGE1 ADC_PTB1
#define STAGE2 ADC_PTB0

// range switches
#define SEL_A GPIO_PTC1
#define SEL_B GPIO_PTC2
#define SEL_C GPIO_PTC3

#define LED1 GPIO_PTD1
#define LED2 GPIO_PTD3

#define PD_EN GPIO_PTA19

// Lowest order bit defines which gain stage to sample.
// Next two bits define which transimpedance range.
enum range {
        RANGE1 = 0, // lowest gain
        RANGE1P,    // lowest gain after secondary gain stage
        RANGE2,
        RANGE2P,
        RANGE3,
        RANGE3P,
        RANGE4,
        RANGE4P,    // highest gain
};

// c,b,a
uint8_t range_muxes[4] = {
        0b000,     // RANGE1
        0b010,     // RANGE2
        0b101,     // RANGE3
        0b001,     // RANGE4
};

static bool autoscale = false;
// Minimum tolerable voltage before increasing gain in microvolts
static uint32_t autoscale_min_thresh = 0.05 * 1e6;
// Maximum tolerable voltage before decreasing gain in microvolts
static uint32_t autoscale_max_thresh = 3.2 * 1e6;
static enum range active_range;

void set_range_mux(enum range rng) {
        active_range = rng;
        if (!amp_on) return;
        uint8_t mux = range_muxes[rng >> 1];
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
                set_range_mux(active_range);
        }
        gpio_write(LED1, on);
        gpio_write(LED2, on);
}

// Forward declarations
void sample_pd_done(uint16_t val, int error, void *cbdata);

static int
start_sample_pd(enum range range)
{
        enum adc_channel channel = range & 1 ? STAGE2 : STAGE1;
        return adc_sample_start(channel, sample_pd_done, (void*) range);
}

static void
show_sample(float avg_codepoint, enum range range)
{
        // ADC voltage in microvolts
        float microvolts = 3.3e6 * avg_codepoint / (1<<16);
        // photodiode current in microamps
        float microamps = 1. * microvolts / active_config.range_gains[range] + active_config.range_offsets[range];
        float sensitivity = interpolate(active_config.sensitivity_lut, active_config.wavelength);
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
        } else if (power > 1e-1) {
                unit = "nano";
                exp = -9;
                real_power = power * 1e3;
        } else {
                unit = "pico";
                exp = -12;
                real_power = power * 1e6;
        }
        printf("%d %luE%d  # %lu %swatts\r\n", range, real_power, exp, real_power, unit);

        if (verbose) {
                uint32_t c = avg_codepoint;
                printf("& %d\t%lu\r\n", range, c);
        }

        if (autoscale && microvolts < autoscale_min_thresh && range != RANGE4P) {
                if (verbose) printf("# moving gain to up range %d\r\n", range+1);
                set_range_mux(range + 1);
        } else if (autoscale && microvolts > autoscale_max_thresh && range != RANGE1) {
                if (verbose) printf("# moving gain to down range %d\r\n", range-1);
                set_range_mux(range - 1);
        }
}

void sample_pd_done(uint16_t val, int error, void *cbdata) {
        enum range range = (enum range) cbdata;
        oversample_accum += val;
        oversample_counter++;
        if (oversample_counter >= oversample)
                show_sample(1. * oversample_accum / oversample_counter, range);
        else
                start_sample_pd(range);
}

int sample_pd(enum range range) {
        set_range_mux(range);
        adc_sample_prepare(ADC_MODE_SAMPLE_LONG | ADC_MODE_POWER_NORMAL | ADC_MODE_AVG_32);
        oversample_accum = 0;
        oversample_counter = 0;
        return start_sample_pd(range);
}

// Address bit 23 of FLASH commands specifies program or data flash
#define DATA_FLASH (1<<23)

int
write_config()
{
        unsigned int offset = 0;
        const char *buf = (const char*) &active_config;
        while (offset < sizeof(struct config)) {
                int res = flash_program_sector(buf + offset, DATA_FLASH | offset, FLASH_SECTOR_SIZE);
                if (res)
                        return res;
                offset += FLASH_SECTOR_SIZE;
        }
        return 0;
}

static struct cdc_ctx cdc;

char cmd_buf[128];
unsigned int tail = 0;

static void
handle_command()
{
        switch (cmd_buf[0]) {
        case 's':
                // Save configuration to FLASH
                {
                        int res = write_config();
                        if (res)
                                printf("# error %d\r\n", res);
                        else
                                printf("# saved\r\n");
                        break;
                }

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
                // Manually set range
                {
                        enum range rng = cmd_buf[0] - '0';
                        set_range_mux(rng);
                        printf("# set range %d\r\n", rng);
                        break;
                }

        case 'g':
                // Configure amplifier gain
                {
                        // range gain in volt/amp
                        char* end;
                        unsigned long range = strtoul(&cmd_buf[1], &end, 10);
                        if (end == &cmd_buf[1] || range >= 8) {
                                printf("# error invalid range\r\n");
                                break;
                        }
                        if (cmd_buf[2] == '=') {
                                unsigned long gain = strtoul(&cmd_buf[3], &end, 10);
                                if (end == &cmd_buf[3]) {
                                        printf("# error invalid gain\r\n");
                                        break;
                                }
                                active_config.range_gains[range] = gain;
                        }
                        unsigned long gain = active_config.range_gains[range];
                        printf("# gain %lu = %lu V/A\r\n", range, gain);
                        break;
                }

        case 'o':
                // Configure amplifier offset
                {
                        // range gain in microamps
                        char* end;
                        unsigned long range = strtoul(&cmd_buf[1], &end, 10);
                        if (end == &cmd_buf[1] || range >= 8) {
                                printf("# error invalid range\r\n");
                                break;
                        }
                        if (cmd_buf[2] == '=') {
                                unsigned long offset = strtoul(&cmd_buf[3], &end, 10);
                                if (end == &cmd_buf[3]) {
                                        printf("# error invalid offset\r\n");
                                        break;
                                }
                                active_config.range_offsets[range] = offset;
                        }
                        unsigned long offset = active_config.range_offsets[range];
                        printf("# offset %lu = %lu microamps\r\n", range, offset);
                        break;
                }

        case 'w':
                // Current wavelength
                {
                        if (cmd_buf[1] == '=') {
                                char* end;
                                unsigned long wl = strtoul(&cmd_buf[2], &end, 10);
                                if (end == &cmd_buf[1]) {
                                        printf("# error 1\r\n");
                                        break;
                                }
                                active_config.wavelength = wl;
                        }
                        printf("# wavelength = %d\r\n", active_config.wavelength);
                        break;
                }

        case 'a':
        case 'A':
                // Enable/disable autoscaling
                autoscale = cmd_buf[0] == 'A';
                printf("# autoscale %s\r\n", autoscale ? "on" : "off");
                break;

        case 'v':
        case 'V':
                // Enable/disable verbose output
                verbose = cmd_buf[0] == 'V';
                printf("# verbose %s\r\n", verbose ? "on" : "off");
                break;

        case '?':
                printf("# OpenPD 0.0 %08lx%08lx%08lx%08lx\r\n",
                       SIM.uidl, SIM.uidml, SIM.uidmh, SIM.uidh);
                break;

        default:
                // Take measurement
                sample_pd(active_range);
        }
}

static void
new_data(uint8_t *data, size_t len)
{
        for (int i=0; i < len; i++) {
                if (data[i] == '\n' || data[i] == '\r') {
                        cmd_buf[tail] = 0;
                        handle_command();
                        tail = 0;
                } else {
                    cmd_buf[tail] = data[i];
                    tail = (tail + 1) % sizeof(cmd_buf);
                }
        }
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

static void
reset_config_to_default()
{
        memcpy(&active_config, &default_config, sizeof(struct config));
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
        set_range_mux(RANGE1);
        set_power(true);

        if (flash_config != NULL && flash_config->magic == CONFIG_MAGIC) {
                memcpy(&active_config, flash_config, sizeof(struct config));
        } else {
                reset_config_to_default();
        }

        usb_init(&cdc_device);
        sys_yield_for_frogs();
}
