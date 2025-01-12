import http.server
import socketserver

class SimpleMirror(socketserver.TCPServer):
    def __init__(self, directory: str):
        self.directory = directory
        super().__init__(("localhost", 0), self._handler)

    @property
    def port(self):
        return self.server_address[1]

    def _handler(self, *args, **kwargs):
        return http.server.SimpleHTTPRequestHandler(*args, directory=self.directory, **kwargs)
