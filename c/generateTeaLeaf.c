#include <stdlib.h>
#include <stdbool.h>
#include <fftw3.h>
#include <stdint.h>
#include "libbmp/libbmp.h"



// SQUARES ARE AESTHETIC
#define NUM_PIXELS 420
#define FREQUENCY_CUTOFF 5


fftw_complex *generateTeaLeaf(uint32_t seed);
bool masked(uint32_t row, uint32_t column);


int32_t main(int32_t argc, char **argv)
{
  fftw_complex *teaLeaf;
  bmp_img img;
  int i, j;


  teaLeaf = generateTeaLeaf(43);

  bmp_img_init_df(&img, NUM_PIXELS, NUM_PIXELS);

  for (i = 0; i < NUM_PIXELS; ++i)
    {
      for (j = 0; j < NUM_PIXELS; ++j)
        {
          if ( teaLeaf[i * NUM_PIXELS + j][0] > NUM_PIXELS * NUM_PIXELS / 2 )
            {
              bmp_pixel_init (&img.img_pixels[i][j], 250, 250, 250);
            } else
            {
              bmp_pixel_init (&img.img_pixels[i][j], 0, 0, 0);
            }
        }
    }

  bmp_img_write (&img, "test.bmp");
	bmp_img_free (&img);

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
