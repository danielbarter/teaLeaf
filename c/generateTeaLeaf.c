#include <stdlib.h>
#include <stdbool.h>
#include <fftw3.h>
#include <stdint.h>


// the bmp header needs to be packed
#define PACKED __attribute__((packed))



// SQUARES ARE AESTHETIC
#define NUM_PIXELS 100
#define FREQUENCY_CUTOFF 5


typedef struct PACKED bit_map_file_header {
  uint16_t bfType; //starts at 0, 2 bytes long
  uint32_t bfSize; //starts at 2, 4 bytes long
  uint16_t bfReserved1; //starts at 6, 2 bytes long
  uint16_t bfReserved2; //starts at 8, 2 bytes long
  uint32_t bfOffBits; //starts at 10, 4 bytes long
} bit_map_file_header;

typedef struct PACKED bit_map_info_header {
  uint32_t biSize;//starts at 14
  int32_t  biWidth;//starts at 18
  int32_t  biHeight;//starts at 22
  uint16_t biPlanes;//starts at 26
  uint16_t biBitCount;//starts at 28
  uint32_t biCompression;//starts at 30
  uint32_t biSizeImage;//starts at 34
  int32_t  biXPelsPerMeter;
  int32_t  biYPelsPerMeter;
  uint32_t biClrUsed;
  uint32_t biClrImportant;
} bit_map_info_header;

fftw_complex *generateTeaLeaf(uint32_t seed);
bool masked(uint32_t row, uint32_t column);



int32_t main(int32_t argc, char **argv)
{
  fftw_complex *teaLeaf = generateTeaLeaf(42);
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
