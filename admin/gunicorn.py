"""
gunicorn configuration
"""

import multiprocessing
import os

if os.environ.get('MODE') == 'dev':
    reload = True

workers = multiprocessing.cpu_count() * 2 + 1
