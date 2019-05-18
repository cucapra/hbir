# -*- coding: utf-8 -*-

project = 'hbir'
copyright = '2019, Cornell University'
author = 'Cornell University'

# The short X.Y version
version = ''
# The full version, including alpha/beta/rc tags
release = ''

# Source options.
extensions = [
]
source_suffix = ['.rst', '.md']
master_doc = 'index'
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'README.md']
pygments_style = None

# HTML output.
html_theme = 'alabaster'
html_static_path = ['_static']

# LaTeX output.
latex_elements = {}
latex_documents = [
    (master_doc, 'hbir.tex', 'hbir Documentation',
     'Cornell University', 'manual'),
]
