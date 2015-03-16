#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import ssl
import threading
import time

import SimpleHTTPServer
import SocketServer
import BaseHTTPServer
import GzipSimpleHTTPServer

def start_http_server(port):
    print "Starting HTTP Server..."
    httpd = SocketServer.TCPServer(("", port), SimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.serve_forever()

def start_https_server(port, certificate):
    print "Starting HTTPS Server..."
    httpd = BaseHTTPServer.HTTPServer(("", port), SimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.socket = ssl.wrap_socket(httpd.socket, certfile=certificate, server_side=True)
    httpd.serve_forever()

def start_gziped_http_server(port):
    print "Starting Gziped HTTP Server..."
    httpd = SocketServer.TCPServer(("", port), GzipSimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.serve_forever()

def start_gziped_https_server(port, certificate):
    print "Starting Gziped HTTPS Server..."
    httpd = BaseHTTPServer.HTTPServer(("", port), GzipSimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.socket = ssl.wrap_socket(httpd.socket, certfile=certificate, server_side=True)
    httpd.serve_forever()

if __name__ == "__main__":
    script_location = os.path.dirname(os.path.abspath(__file__))
    site_location = os.path.join(script_location, "test-data", "site")
    certfile = os.path.join(script_location, "test-data", "certificates", "self-ssl.pem")
    os.chdir(site_location)

    print "Serving", os.getcwd(), "directory"

    server_threads = [threading.Thread(target = start_http_server, args = (8001,)),
                      threading.Thread(target = start_https_server, args = (4443, certfile)),
                      threading.Thread(target = start_gziped_http_server, args = (8002,)),
                      threading.Thread(target = start_gziped_https_server, args = (4444, certfile))]

    for thread in server_threads:
        thread.daemon = True
        thread.start()

    while True:
        time.sleep(1)
