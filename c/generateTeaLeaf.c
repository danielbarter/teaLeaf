#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <fftw3.h>
#include <stdint.h>

#define PACKED __attribute__((packed))

// SQUARES ARE AESTHETIC
#define NUM_PIXELS 420
#define FREQUENCY_CUTOFF 5


typedef struct _bmp_header
{
  uint16_t                bfheader; // 0
	uint32_t                bfSize;  // 2
	uint32_t                bfReserved; // 6
	uint32_t                bfOffBits; // 10
	uint32_t                biSize; // 14
	int32_t                 biWidth; // 18
	int32_t                 biHeight; // 22
	uint16_t                biPlanes; // 26
	uint16_t                biBitCount; // 28
	uint32_t                biCompression; // 30
	uint32_t                biSizeImage; // 34
	int32_t                 biXresolution; // 38
	int32_t                 biYresolution; // 42
	uint32_t                biColorUsed; // 46
	uint32_t                biColorImportant; // 50
} PACKED bmp_header;

typedef struct _bmp_pixel
{
  unsigned char blue;
	unsigned char green;
	unsigned char red;
	unsigned char alpha;
} bmp_pixel;




fftw_complex *generateTeaLeaf(uint32_t seed);
bool masked(uint32_t row, uint32_t column);


int32_t main(int32_t argc, char **argv)
{

  if (argc != 2)
    {
      puts("usage: generateTeaLeaf.elf (argument)");
      puts("expected one argument");
      return 1;
    }

  fftw_complex *teaLeaf;
  int i;
  uint32_t seed = 0;
  uint8_t *input_array = (uint8_t *) argv[1];
  size_t length = strlen(argv[1]);

  for (i = 0; i < length; ++i)
    {
      seed = seed + (uint32_t)input_array[i];
    }

  teaLeaf = generateTeaLeaf(seed);
  bmp_pixel image[NUM_PIXELS * NUM_PIXELS];
  bmp_header header =
    { .bfheader = 0x4D42,
      .bfSize = sizeof(bmp_header) + NUM_PIXELS * NUM_PIXELS * sizeof(bmp_pixel),
      .bfReserved = 0,
      .bfOffBits = sizeof(bmp_header),
      .biSize = 40,
      .biWidth = NUM_PIXELS,
      .biHeight = NUM_PIXELS,
      .biPlanes = 1,
      .biBitCount = 32,
      .biCompression = 0,
      .biSizeImage = NUM_PIXELS * NUM_PIXELS,
      .biXresolution = 0,
      .biYresolution = 0,
      .biColorUsed = 0,
      .biColorImportant = 0
    };

  for (i = 0; i < NUM_PIXELS * NUM_PIXELS; ++i)
    {
          if ( teaLeaf[i][0] > NUM_PIXELS * NUM_PIXELS / 2 )
            {
              image[i].blue = (uint8_t)0xf9;
              image[i].green = (uint8_t)0x66;
              image[i].red = (uint8_t)0x54;
              image[i].alpha = (uint8_t)255;
            } else
            {
              image[i].blue = (uint8_t)255;
              image[i].green = (uint8_t)255;
              image[i].red = (uint8_t)255;
              image[i].alpha = (uint8_t)0;
            }
    }
  FILE *img_file = fopen("tealeaf.bmp","w");
  fwrite((void *) &header, sizeof(bmp_header),1,img_file);
  fwrite((void *) image, sizeof(bmp_pixel)*NUM_PIXELS*NUM_PIXELS,1,img_file);

  return 0;
}


bool masked(uint32_t row, uint32_t column)
{
  return (FREQUENCY_CUTOFF <= row && row <= NUM_PIXELS - FREQUENCY_CUTOFF)
    || (FREQUENCY_CUTOFF <= column && column <= NUM_PIXELS - FREQUENCY_CUTOFF);
}

fftw_complex *generateTeaLeaf(uint32_t seed)
{
  fftw_complex *in, *middle, *out;
  fftw_plan plan1;
  fftw_plan plan2;
  uint32_t i;
  uint32_t row, column;

  srand(seed);


  // it would be nice to statically allocate these arrays, but the
  // fftw malloc makes sure everything is alligned right for simd
  in     = fftw_alloc_complex(NUM_PIXELS * NUM_PIXELS);
  middle = fftw_alloc_complex(NUM_PIXELS * NUM_PIXELS);
  out    = fftw_alloc_complex(NUM_PIXELS * NUM_PIXELS);

  // plans need to be created before initialization
  plan1 = fftw_plan_dft_2d(NUM_PIXELS,
                           NUM_PIXELS,
                           in, middle,
                           FFTW_FORWARD,
                           FFTW_ESTIMATE);

  plan2 = fftw_plan_dft_2d(NUM_PIXELS,
                           NUM_PIXELS,
                           middle, out,
                           FFTW_BACKWARD,
                           FFTW_ESTIMATE);

  for (i = 0; i < NUM_PIXELS * NUM_PIXELS; ++i)
    {
      in[i][0] = (double) (rand() % 2); // initialize real part
      in[i][1] = 0.0; // initialize complex part
    }

  fftw_execute(plan1);

  for (i = 0; i < NUM_PIXELS * NUM_PIXELS; ++i)
    {
      row = i / NUM_PIXELS;
      column = i % NUM_PIXELS;
      if ( masked(row,column) )
        {
        middle[i][0] = 0.0;
        middle[i][1] = 0.0;
        }
    }

  fftw_execute(plan2);

  fftw_destroy_plan(plan1);
  fftw_destroy_plan(plan2);
  fftw_free(in);
  fftw_free(middle);

  return out;
}
