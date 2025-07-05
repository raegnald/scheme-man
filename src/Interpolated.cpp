#include "Interpolated.hpp"

float interpolate(Interpolating_function type, float x) {
  switch (type) {
  case Linear:
    return x;
  case Ease_out_quad:
    return 1 - (1 - x) * (1 - x);
  case Ease_out_elastic: {
    const float c4 = (2 * PI) / 3;
    return (x == 0)   ? 0
           : (x == 1) ? 1
                      : pow(2, -10 * x) * sin((x * 10 - 0.75) * c4) + 1;
  }
  }
}
