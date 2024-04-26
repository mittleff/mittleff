(use-modules (pyffi))

(python-initialize)

(pyimport matplotlib.pyplot)
(pyimport numpy)

(let* ((filename "test.png")
       (t (#.numpy.arange 0.0 (* 2 #.numpy.pi) 0.01))
       (s (#.numpy.sin t))
       (fig-ax-tuple (#.matplotlib.pyplot.subplots))
       (fig (vector-ref fig-ax-tuple 0))
       (ax (vector-ref fig-ax-tuple 1)))
  (#.ax.plot t s)
  (#.ax.grid)
  (#.ax.set #:xlabel "time (s)" #:ylabel "voltage (mV)" #:title "About as simple as it gets, folks")
  (#.fig.savefig filename))

(python-finalize)
