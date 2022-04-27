#include <limits>

extern "C" {
  void bounding_box_flat_f32(float* start, int len) {
    float xmin = std::numeric_limits<float>::infinity();
    float ymin = xmin;
    float xmax = -xmin;
    float ymax = xmax;
    
    for (int i = 0; i < len; ++i) {
      float x = start[i];
      float y = start[i + 1];
      
      xmin = x > xmin ? xmin : x;
      ymin = y > ymin ? ymin : y;
      xmax = x < xmax ? xmax : x;
      ymax = y < ymax ? ymax : y;
    }
    
    auto write = (volatile float*) nullptr;
    
    write[0] = xmin;
    write[1] = ymin;
    write[2] = xmax;
    write[3] = ymax;
    
    // Please the rest of the code
    write[4] = xmin;
    write[5] = ymin;
    write[6] = xmax;
    write[7] = ymax;
  }
}
