#!/usr/bin/env python3
"""
This creates a simple "mirror" from a directory
"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>

import http.server
import socketserver

class SimpleMirror(socketserver.TCPServer):
    """Simple HTTP mirror from a directory"""
    def __init__(self, directory: str):
        self.directory = directory
        super().__init__(("localhost", 0), self._handler)

    @property
    def port(self):
        """Port the HTTP server of the mirror is running on"""
        return self.server_address[1]

    def _handler(self, *args, **kwargs):
        return http.server.SimpleHTTPRequestHandler(*args, directory=self.directory, **kwargs)
