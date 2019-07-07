# -*- coding: utf-8 -*-

project = 'HBIR'
copyright = '2019, Cornell University'
author = 'Cornell University'

# The short X.Y version
version = ''
# The full version, including alpha/beta/rc tags
release = ''

extensions = [
    'recommonmark',
    'sphinxcontrib.katex',
]

# Source options.
source_suffix = ['.rst', '.md']
master_doc = 'index'
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'README.md']
pygments_style = None

# HTML output.
html_theme = 'alabaster'
html_theme_options = {
    'description': 'A programming language & compiler target for '
        'continuously reconfigurable machines.',
    'extra_nav_links': {
        'Source on GitHub': 'https://github.com/cucapra/hbir',
    },
}
html_static_path = ['_static']
html_css_files = ['style.css']

# KaTeX rendering.
katex_inline = ['$', '$']

# LaTeX output.
latex_elements = {}
latex_documents = [
    (master_doc, 'hbir.tex', 'hbir Documentation',
     'Cornell University', 'manual'),
]
