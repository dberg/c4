#------------------------------------------------------------------------------
# C4 Dockerfile for Fedora
#------------------------------------------------------------------------------
# Build image:
#   docker build -t="USER/c4:v1" .
#
# Run:
#   docker run -t -i --name c4 -v `pwd`:/root/c4 USER/c4:v1 bash
#------------------------------------------------------------------------------
FROM fedora:22
MAINTAINER Dani Berg <danibberg@gmail.com>

RUN dnf -y update
RUN yum -y install emacs-nox gcc-c++ gdb git make protobuf-devel strace

