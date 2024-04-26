(use-modules (pyffi))

(python-initialize)
(pyimport urllib.request)

(let* ((url "http://www.gnu.org/licenses/gpl-3.0.txt")
       (res (#.urllib.request.urlopen url))
       (content (#.res.read)))
  (display (#.content.decode))
  (newline)
  (#.res.close))
(python-finalize)
