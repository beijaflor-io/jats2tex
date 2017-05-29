.. _elemento-boxed-text:

<boxed-text>
============

Aparece em:

  :ref:`elemento-app`
  ``<app-group>``
  :ref:`elemento-body`
  :ref:`elemento-boxed-text`
  :ref:`elemento-glossary`
  :ref:`elemento-p`
  :ref:`elemento-ref-list`
  :ref:`elemento-sec`


Atributos obrigatórios:

  1. ``@id`` (ver :ref:`sugestao-atribuicao-id`)

Ocorre:

  Zero ou mais vezes

Identifica texto ou objeto que faz parte do artigo, mas não do corpo do texto (flutuante ou não ancorado). Permite apresentar texto com seções, figuras, tabelas etc.


Exemplos:

  * :ref:`elemento-boxtext-exemplo-1`
  * :ref:`elemento-boxtext-exemplo-2`


.. _elemento-boxtext-exemplo-1:

Exemplo de ``<boxed-text>`` com seção:
--------------------------------------

.. code-block:: xml

    ...
    <boxed-text id="bx1">
        <sec>
            <title>Box 1 Use of antidepressants and suicidality in young individuals</title>
            <p>In 2004, the FDA made the decision to include a black box warning about the risk of suicidality associated with antidepressant use among individuals under 25 years of age<xref ref-type="bibr" rid="B26">26</xref>.</p>
            <p>Based on the fact that for the treatment of depressive disorders the number needed to treat (NNT) in this age group is at least 10 and the number needed to harm (number needed to harm, NNH) is 112, it was concluded that the benefits associated with the use of antidepressants outweigh the potential risks.</p>
        </sec>
    </boxed-text>
    ...



.. _elemento-boxtext-exemplo-2:

Exemplo de ``<boxed-text>`` com figura:
---------------------------------------

.. code-block:: xml

    ...
    <boxed-text id="bx1">
        <fig id="f12">
            <label>Figure</label>
            <caption>
                <title>Diagnostic algorithm for depressive episodes in children and adolescents</title>
            </caption>
            <graphic xlink:href="1516-4446-rbp-1516-4446-2012-S0022-gf01.jpg"/>
        </fig>
    </boxed-text>
    ...


.. {"reviewed_on": "20160623", "by": "gandhalf_thewhite@hotmail.com"}
