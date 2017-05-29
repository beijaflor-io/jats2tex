.. _elemento-pub-date:

<pub-date>
==========

Aparece em:

  :ref:`elemento-article-meta`

Atributos obrigatórios:

  1. ``@pub-type``

Ocorre:

  Uma vez


A data de publicação do artigo/número utiliza o elemento ``<pub-date>``, o qual pode conter os elementos :ref:`elemento-day`, :ref:`elemento-month`, :ref:`elemento-season` e, obrigatoriamente, :ref:`elemento-year`.

``<pub-date>`` deve estar acompanhada do atributo ``@pub-type``, que pode ser ``epub-ppub`` se houver uma versão impressa do número ou apenas ``epub`` quando: a. for apenas publicação online; b. quando se tratar de um documento *Rolling Pass* ou c. em caso de ser modalidade :term:`ahead of print`.

Exemplos:

    * :ref:`elemento-pubdate-exemplo-1`
    * :ref:`elemento-pubdate-exemplo-2`
    

.. _elemento-pubdate-exemplo-1: 

Exemplo de ``<pub-date>`` nas versões impressa e digital com ``<season>``:
--------------------------------------------------------------------------

.. code-block:: xml

    ...
    <article-meta>
        ...
        <pub-date pub-type="epub-ppub">
            <season>Jan-Feb</season>
            <year>2014</year>
        </pub-date>
        ...
    </article-meta>
    ...

.. _elemento-pubdate-exemplo-2: 

Exemplo de ``<pub-date>`` nas versões impressa e digital com mês e dia:
-----------------------------------------------------------------------

Os valores de dia, mês e ano devem ser representados de acordo com a data de publicação do número, geralmente constante no sumário.

.. code-block:: xml

    ...
    <article-meta>
        ...
        <pub-date pub-type="epub-ppub">
            <day>21</day>
            <month>07</month>
            <year>2016</year>
        </pub-date>
        ...
    </article-meta>
    ...


.. _elemento-pubdate-exemplo-3:

Exemplo de ``<pub-date>`` na versão digital:
--------------------------------------------

.. code-block:: xml

    ...
    <article-meta>
        ...
        <pub-date pub-type="epub">
            <day>17</day>
            <month>03</month>
            <year>2014</year>
        </pub-date>
        ...
    </article-meta>
    ...


.. {"reviewed_on": "20160803", "by": "gandhalf_thewhite@hotmail.com"}



LaTeX
-----

  .. code-block:: tex

      ...
      % Defined at scielostyle.sty
       \renewcommand{\xmlpubdatestyle}{\color{out}\bfseries}
       \RenewDocumentCommand{\xmlpubdate}{ O{} O{} O{} O{} m }{{\xmlpubdatestyle%
                               \ifx#1\relax\else (#1)\ \fi %  <season> (non mandatory)
                               \ifx#2\relax\else #2--\fi % <month> (non mandatory)
                               \ifx#3\relax\else #3--\fi % <day> (non mandatory)
                               #5 % <year>
                               \ifx#4\relax\else~(#4)\fi}} % <date pub-type="epub"> (non mandatory)


      \xmlpubdate{2015}                % year

      \xmlpubdate[FEV-MAR]{2015}       % season-year

      \xmlpubdate[FEV MAR][12]{2015}   % month-year

      \xmlpubdate[][12][25]{2015}      % day-month-year

      \xmlpubdate[][][][epub]{2015}    % year+@pub-type
      ...

.. {"reviewed_on": "20161223", "by": "jorge@hedra.com.br"}



