  import os
   from distutils.core import setup
   from distutils.extension import Extension
   from Cython.Distutils import build_ext
   import numpy as np
   
   eca = ['-arch', 'x86_64']
   ela = eca
   
  setup(
          cmdclass = {'build_ext':build_ext},
          include_dirs=[np.get_include()],
          ext_modules = [Extension("example",["example.pyx"],
                                                              libraries=["m"],
                                                              extra_objects = ["src/example_wrapper.o","src/example.o"],
                                                              extra_compile_args = eca,
                                                              extra_link_args = ela,
                                                              language="c++")]
       )
~            
