.. _elemento-role:

<role>
======

Aparece em:

  :ref:`elemento-collab`
  :ref:`elemento-contrib`
  :ref:`elemento-contrib-group`
  :ref:`elemento-element-citation`
  :ref:`elemento-person-group`
  :ref:`elemento-product`

Ocorre:

  Zero ou mais vezes


``<role>`` (função ou papel) é usado para especificar o tipo de responsabilidade (ou função) do contribuinte do :term:`artigo`.

Exemplos:

    * :ref:`elemento-role-exemplo-1`
    * :ref:`elemento-role-exemplo-2`



.. _elemento-role-exemplo-1:

Exemplo de ``<role>`` em ``<contrib>``:
---------------------------------------

.. code-block:: xml

    ...
    <contrib contrib-type="author">
        ...
        <name>
            <surname>Meader</surname>
            <given-names>CR</given-names>
            <prefix>Dr.</prefix>
            <suffix>Junior</suffix>
        </name>
        <xref ref-type="aff" rid="aff02">2</xref>
        <role>Pesquisador</role>
        ...
    </contrib>
    ...

.. _elemento-role-exemplo-2:

Exemplo de ``<role>`` em ``<element-citation>``:
------------------------------------------------

.. code-block:: xml

    ...
    <element-citation publication-type="journal">
        ...
        <person-group person-group-type="author">
            <name>
                <surname>Petitti</surname>
                <given-names>DB</given-names>
                ...
            </name>
            <name>
                <surname>Crooks</surname>
                <given-names>VC</given-names>
                ...
            </name>
            <role>pesquisador</role>
            ...
        </person-group>
        ...
    </element-citation>
    ...

.. {"reviewed_on": "20160628", "by": "gandhalf_thewhite@hotmail.com"}
