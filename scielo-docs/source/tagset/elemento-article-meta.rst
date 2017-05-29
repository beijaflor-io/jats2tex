.. _elemento-article-meta:

<article-meta>
==============

Aparece em:

  :ref:`elemento-front`

Ocorre:

  Uma vez


Contém os metadados do artigo, cujos elementos básicos são:

* :term:`DOI`;
* seção (de acordo com o sumário do periódico);
* título(s) do artigo;
* autor(es);
* afiliação e notas de autor ``*``;
* data de publicação;
* volume ``*``;
* número ``*``;
* paginação do artigo ``*``;
* resumo(s) ``*``;
* palavras-chave ``*``;
* histórico ``*``;
* indicação da licença de uso :term:`Creative Commons`;
* contagem de elementos ``*``; e
* dados de financiamento  ``*``.

.. note:: Os itens identificados com asterisco devem ser inseridos no :term:`documento` somente se aplicável.


.. {"reviewed_on": "20160728", "by": "gandhalf_thewhite@hotmail.com"}

LaTeX
-----

  .. code-block:: tex

        % Defined at scielostyle.sty
        \renewcommand{\articleidpubidtypedoi}[1]{#1}
        \renewcommand{\volume}[1]{#1}
        \renewcommand{\issue}[1]{#1}
        \renewcommand{\fpage}[1]{#1}
        \renewcommand{\lpage}[1]{#1}

        %      <article-meta> ...
        \renewenvironment{articlemeta}{\begin{tcolorbox}[title=<article-meta>]}{\end{tcolorbox}}
        %      <article-categories> ...
        \renewenvironment{articlecategories}{\begin{tcolorbox}[title=<article-categories>]}{\end{tcolorbox}}
        %      <title-group> ...
        \renewenvironment{titlegroup}{\begin{tcolorbox}[title=<title-group>]}{\end{tcolorbox}}
        %      <contrib-group> ...
        \renewenvironment{contribgroup}{\begin{tcolorbox}[title=<contrib-group>]}{\end{tcolorbox}}
        %      <author-notes> ...
        \renewenvironment{authornotes}{\begin{tcolorbox}[title=<author-notes>]}{\end{tcolorbox}}
        %      <pub-date> ...
        \renewenvironment{pubdate}{\begin{tcolorbox}[title=<pub-date>]}{\end{tcolorbox}}
        %      <history> ...
        \renewenvironment{history}{\begin{tcolorbox}[title=<history>]}{\end{tcolorbox}}
        %      <permissions> ...
        \renewenvironment{permissions}{\begin{tcolorbox}[title=<permissions>]}{\end{tcolorbox}}
        %      <abstract> ...
        \renewenvironment{abstract}{\begin{tcolorbox}[title=<absctract>]}{\end{tcolorbox}}
        %      <kwd-group> ...
        \renewenvironment{kwdgroup}{\begin{tcolorbox}[title=<kwd-group>]}{\end{tcolorbox}}
        %      <counts> ...
        \renewenvironment{counts}{\begin{tcolorbox}[title=<counts>]}{\end{tcolorbox}}


        % <article-meta>
       \begin{articlemeta}
        %      <article-id pub-id-type="doi">
              \articleidpubidtypedoi{10.1590/1677-3225v14n4a01}

        %      <article-categories> ...
                \begin{articlecategories}
                ...    
                \end{articlecategories}
        %      <title-group> ...
                \begin{titlegroup}
                ...
                \end{titlegroup}
        %      <contrib-group> ...
                \begin{contribgroup}
                ...
                \end{contribgroup}
        %      <author-notes> ...
                \begin{authornotes}
                ...
                \end{authornotes}
        %      <pub-date> ...
                \begin{pubdate}
                ...
                \end{pubdate}
        %      <volume>
                \volume{14}

        %      <issue>
                \issue{4}

        %      <fpage>
                \fpage{256}

        %      <lpage>
                \lpage{261}

        %      <history> ...
                \begin{history}
                ...
                \end{history}
        %      <permissions> ...
                \begin{permissions}
                ...
                \end{permissions}
        %      <abstract> 
                \begin{abstract}
                ...
                \end{abstract}         
        %      <kwd-group> ...
                \begin{kwdgroup}
                ...
                \end{kwdgroup}
        %      <counts> ...
                \begin{counts}
                ...
                \end{counts}
        % </article-meta>
        \end{articlemeta}

.. {"reviewed_on": "20161224", "by": "jorge@hedra.com.br"}
