(use-modules (pyffi))

(python-initialize)

(pyimport http.server)
(pyimport socketserver)

(let* ((handler #.http.server.SimpleHTTPRequestHandler)
       (httpd (#.socketserver.TCPServer #("" 8080) handler)))
  (#.httpd.serve-forever))

(python-finalize)
