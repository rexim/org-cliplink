;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'el-mock)
(require 'undercover)

(undercover "*.el")

(add-to-list 'load-path ".")
(load "org-cliplink.el")
