# Grammar Fusion

Merging ABNF grammars that were generated using a buttom-up and a top-down approach, and evaluating the original and resulting grammars with respect to a ground truth.

Run with:

        python start.py input_BU input_TD groundtruth


Fusion Strategies:
------------------

* Late fusion (simple union of the two grammars)
* Mid-level fusion (coming soon)
* Early fusion (coming soon)

Dependencies:
-------------

* pdflatex
