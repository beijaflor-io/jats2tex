.. _elemento-kwd-group:

<kwd-group>
===========

Aparece em:

  :ref:`elemento-article-meta`

Atributos obrigatórios:

  1. ``@xml:lang``

Ocorre:

  Zero ou mais vezes


Identifica o grupo de palavras-chave do artigo por idioma. Contém, obrigatoriamente, o atributo ``@xml:lang``. ``<kwd-group>`` deve ter ainda um título identificando o grupo por meio do elemento ``title``.

.. code-block:: xml

    ...
    <article-meta>
        ...
        <kwd-group xml:lang="pt">
            <title>Palavra-chave</title>
            <kwd>Broncoscopia</kwd>
        </kwd-group>
        ...
    </article-meta>
    ...


LaTeX
-----

  .. code-block:: tex
 
      % Defined at scielostyle.sty
      \renewcommand{\kwdgroupbeforeskip}{\medskip}
      \renewcommand{\kwdstyle}{\itshape\color{out}}
      \renewcommand{\kwdtitle}{Keywords:}
      \renewcommand{\kwdtitlestyle}{\noindent\bfseries\color{out}}
      \renewcommand{\kwd}[1]{{\kwdtitlestyle
                              \kwdtitle} 
                              \kwdstyle #1}
      \renewcommand{\kwdgroupbeforeskip}{\medskip}
      \renewenvironment{kwdgroup}{\kwdgroupbeforeskip}{}
      
      ...
      \begin{kwdgroup}
      \kwd{Chagas disease, Quality of life, Health-related quality of 
      life, Cardiomyopathy, Determining factors}
      \end{kwdgroup}

      ...

.. {"reviewed_on": "20161224", "by": "jorge@hedra.com.br"}
