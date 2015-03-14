#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import ssl
import threading

import SimpleHTTPServer
import SocketServer
import BaseHTTPServer

def start_http_server(port):
    httpd = SocketServer.TCPServer(("", port), SimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.serve_forever()

def start_https_server(port, certificate):
    httpd = BaseHTTPServer.HTTPServer(("", port), SimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.socket = ssl.wrap_socket(httpd.socket, certfile=certificate, server_side=True)
    httpd.serve_forever()

if __name__ == "__main__":
    script_location = os.path.dirname(os.path.abspath(__file__))
    site_location = os.path.join(script_location, "test-data", "site")
    certfile = os.path.join(script_location, "test-data", "certificates", "self-ssl.pem")
    os.chdir(site_location)

    print "Serving", os.getcwd(), "directory"

    server_threads = [threading.Thread(target = start_http_server, args = (8001,)),
                      threading.Thread(target = start_https_server, args = (4443, certfile))]

    for thread in server_threads:
        thread.start()

    for thread in server_threads:
        thread.join()
