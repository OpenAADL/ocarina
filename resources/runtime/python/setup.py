#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Setup file for Linux distribution
Usage:  python setup.py sdist   --> to create a tarball
        python setup.py install --> to install in python directory
'''
# from distutils.core import setup

from setuptools import setup, find_packages

import ocarina

setup(
    name='ocarina',
    version=ocarina.__version__,
    packages=find_packages(),
    author='Ellidiss/ISAE',
    author_email='taste@ellidiss.fr',
    description='Python bindings for Ocarina',
    long_description=open('README.md').read(),
    include_package_data=True,
    url='http://taste.tuxfamily.org',
    classifiers=[
        'Programming Language :: Python',
        'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2.7'
    ]
)
