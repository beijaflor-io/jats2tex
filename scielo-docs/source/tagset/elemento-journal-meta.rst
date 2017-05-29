.. _elemento-journal-meta:

<journal-meta>
==============

Aparece em:

  :ref:`elemento-front`

Ocorre:

  Uma vez


Em ``<journal-meta>`` são identificados os metadados do periódico.

.. note:: Sugere-se consultar o :ref:`arquivo de metadados dos periódicos <journal-meta-csv>` como referência na identificação dos elementos.

Exemplo:

.. code-block:: xml

   ...

   <journal-meta>
        <journal-id journal-id-type="nlm-ta">Braz J Med Biol Res</journal-id>
        <journal-id journal-id-type="publisher-id">bjmbr</journal-id>
        <journal-title-group>
             <journal-title>Brazilian Journal of Medical and Biological Research</journal-title>
             <abbrev-journal-title abbrev-type="publisher">Braz. J. Med. Biol. Res.</abbrev-journal-title>
        </journal-title-group>
        <issn pub-type="epub">1414-431X</issn>
        <issn pub-type="ppub">0100-879X</issn>
        <publisher>
             <publisher-name>Associação Brasileira de Divulgação Científica</publisher-name>
        </publisher>
   </journal-meta>
   ...


.. {"reviewed_on": "20160626", "by": "gandhalf_thewhite@hotmail.com"}


LaTeX
-----

  .. code-block:: tex
 
      % <journal-meta>
      \renewenvironment{journalmeta}{\begin{tcolorbox}[title=<journal-meta>]}{\end{tcolorbox}}
      % <journal-id journal-id-type="nlm-ta">
             \renewcommand{\journalidtypenlmstyle}{\itshape}
             \renewcommand{\journalidtypenlm}[1]{{\journalidtypenlmstyle #1}}
      % <journal-id journal-id-type="publisher-id">
             \renewcommand{\journalidtypeublisheridstyle}{\itshape}
             \renewcommand{\journalidtypeublisherid}[1]{{\journalidtypeublisheridstyle #1}}
      % <journal-title-group>
      \renewenvironment{journaltitlegroup}{\begin{tcolorbox}[colback=red!5!white,title=<journal-title-group>]}{\end{tcolorbox}}
      %       % <journal-title>
                  \renewcommand{\journaltitlestyle}{\itshape}
                  \renewcommand{\journaltitle}[1]{{\journaltitlestyle #1}}
      %       % <abbrev-journal-title abbrev-type="publisher">
                  \renewcommand{\abbrevjournaltitlepublisherstyle}{\itshape}
                  \renewcommand{\abbrevjournaltitlepublisher}[1]{{\abbrevjournaltitlepublisherstyle #1}}
      % <issn pub-type="epub">
            \renewcommand{\issnepubstyle}{\itshape}
            \renewcommand{\issnepub}[1]{{\issnepubstyle #1}}
      % <issn pub-type="ppub">
            \renewcommand{\issnppubstyle}{\itshape}
            \renewcommand{\issnppub}[1]{{\issnppubstyle #1}}
      % <publisher>
            \renewenvironment{publisher}{\begin{tcolorbox}[colback=green!5!white,title=<publisher>]}{\end{tcolorbox}}
            % <publisher-name>
                  \renewcommand{\publishernamestyle}{\itshape}
                  \renewcommand{\publishername}[1]{{\publishernamestyle #1}}

      ...

      % <journal-meta>
      \begin{journalmeta}
            % <journal-id journal-id-type="nlm-ta">
            \journalidtypenlm{Braz J Med Biol Res}

            % <journal-id journal-id-type="publisher-id">
            \journalidtypeublisherid{bjmbr}

            % <journal-title-group>
            \begin{journaltitlegroup}
                 % <journal-title>
                 \journaltitle{Brazilian Journal of Medical and Biological Research}
      
                 % <abbrev-journal-title abbrev-type="publisher">
                 \abbrevjournaltitlepublisher{Braz. J. Med. Biol. Res.}
           
            \end{journaltitlegroup}

           % <issn pub-type="epub">
           \issnepub{1414-431X}
           % <issn pub-type="ppub">
           
           \issnppub{0100-879X}

           % <publisher>
            \begin{publisher}
                % <publisher-name>
                \publishername{Associação Brasileira de Divulgação Científica}
            \end{publisher}

      \end{journalmeta}
      ...

.. {"reviewed_on": "20161224", "by": "jorge@hedra.com.br"}


