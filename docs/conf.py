extensions = [
    'sphinx_a4doc',
    "breathe",
    "sphinx.ext.autosectionlabel",
]

from os.path import dirname
a4_base_path = dirname(__file__) + '/../src/parse/antlr/'

html_sidebars = {
    '**': [
        "navbar-logo.html",
        "search-field.html",
        "sbt-sidebar-nav.html",
    ]
}

html_theme_options = {
    "repository_url": "https://gitlab.sai.jku.at/booleguru/booleguru",
    "use_repository_button": True,
}

# Breathe Configuration
breathe_default_project = "Booleguru"

html_theme = 'sphinx_book_theme'

# Sphinx Configuration
project = "Booleguru"
author = "Max Heisinger"
copyright = "2023, Max Heisinger"
html_title = "Booleguru Docs"
