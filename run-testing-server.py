#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import ssl
import threading
import time

import SimpleHTTPServer
import BaseHTTPServer
import GzipSimpleHTTPServer
import SimpleAuthHandler


def start_http_server(port):
    print "(%d) Starting HTTP Server..." % port
    httpd = BaseHTTPServer.HTTPServer(("", port), SimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.serve_forever()


def start_https_server(port, certificate):
    print "(%d) Starting HTTPS Server..." % port
    httpd = BaseHTTPServer.HTTPServer(("", port), SimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.socket = ssl.wrap_socket(httpd.socket, certfile=certificate, server_side=True)
    httpd.serve_forever()


def start_gziped_http_server(port):
    print "(%d) Starting Gziped HTTP Server..." % port
    httpd = BaseHTTPServer.HTTPServer(("", port), GzipSimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.serve_forever()


def start_gziped_https_server(port, certificate):
    print "(%d) Starting Gziped HTTPS Server..." % port
    httpd = BaseHTTPServer.HTTPServer(("", port), GzipSimpleHTTPServer.SimpleHTTPRequestHandler)
    httpd.socket = ssl.wrap_socket(httpd.socket, certfile=certificate, server_side=True)
    httpd.serve_forever()


def start_http_server_with_basic_auth(port):
    print "(%d) Starting HTTP Server with Basic Auth" % port
    httpd = BaseHTTPServer.HTTPServer(("", port), SimpleAuthHandler.SimpleAuthHandler)
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
                      threading.Thread(target = start_gziped_https_server, args = (4444, certfile)),
                      threading.Thread(target = start_http_server_with_basic_auth, args = (8003,))]

    for thread in server_threads:
        thread.daemon = True
        thread.start()

    # This loop allows you to ^C when you run this script standalone
    while True:
        time.sleep(1)
