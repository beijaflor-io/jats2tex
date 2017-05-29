.. _elemento-label:

<label>
=======

Aparece em:

  :ref:`elemento-aff`
  :ref:`elemento-app`
  :ref:`elemento-boxed-text`
  :ref:`elemento-corresp`
  :ref:`elemento-def-list`
  :ref:`elemento-disp-formula`
  :ref:`elemento-fig`
  :ref:`elemento-fn`
  ``glossary``
  :ref:`elemento-list`
  ``list-item``
  :ref:`elemento-media`
  :ref:`elemento-ref`
  :ref:`elemento-supplementary-material`
  :ref:`elemento-table-wrap`  
  :ref:`elemento-verse-group`
  

Ocorre:

  Zero ou mais vezes


``<label>`` tem como função identificar, numérica e/ou alfabéticamente, um grupo de elementos de tipo específico.

Exemplos:

 * :ref:`elemento-label-exemplo-1`
 * :ref:`elemento-label-exemplo-2`
 * :ref:`elemento-label-exemplo-3`
 * :ref:`elemento-label-exemplo-4`
 * :ref:`elemento-label-exemplo-5`
 * :ref:`elemento-label-exemplo-6`


.. _elemento-label-exemplo-1:

Exemplo em ``<aff>``:
---------------------


.. code-block:: xml

    <aff id="aff01">
        <label>a</label>
        ...
    </aff>



.. _elemento-label-exemplo-2:

Exemplo em ``<corresp>``:
-------------------------

.. code-block:: xml

    <corresp id="c01">
       <label>*</label>
       ...
    </corresp>


.. _elemento-label-exemplo-3:

Exemplo em ``<fig>``:
---------------------

.. code-block:: xml

    <fig id="f01">
        <label>Figure 1</label>
        ...
    </fig>


.. _elemento-label-exemplo-4:

Exemplo em ``<table-wrap>``:
----------------------------

.. code-block:: xml

    <table-wrap id="t01">
        <label>Table 1</label>
        ...
    </table-wrap>


.. _elemento-label-exemplo-5:

Exemplo em ``<ref>``:
---------------------

.. code-block:: xml

    <ref id="B01">1</ref>
        <label>1</label>
        ...
    </ref>


.. _elemento-label-exemplo-6:

Exemplo em ``<app>``:
---------------------

.. code-block:: xml

    <app id="app01">
        <label>Apêndice</label>
        ...
    </app>


.. {"reviewed_on": "20160627", "by": "gandhalf_thewhite@hotmail.com"}
