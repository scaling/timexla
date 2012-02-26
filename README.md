# Timexla

**Timexla** is a temporal expression detector, extractor, and resolver.

## Yet another timex engine?

It seeks to be:

1. less pre-packaged than [SUTime](http://nlp.stanford.edu/software/corenlp.shtml), Stanford NLP's temporal expression recognizer, which is great but comes rolled into the whole 100+ MB CoreNLP package.
2. less rule-based than [HeidelTime](http://dbs.ifi.uni-heidelberg.de/index.php?id=129), which is state of the art, but operates on a lot of effective, but hard-coded rules.

Two improvements, which are novel in the field as far as I know, are to:

1. Power (temporal) anaphora resolution by carrying along an anchor/center of temporal location, much as one does with pronoun anaphora.
2. Adjusting priors (leaning forward or backward) depending on tense as well as aspect of verbs connected to 

## Data

You'll need the [TimeML](http://www.ldc.upenn.edu/Catalog/CatalogEntry.jsp?catalogId=LDC2006T08) corpus to run the code as-is. Fortunately, this is free, or something vaguely resembling free (despite the $0 total), because you have to petition the LDC for it.

## Contributors

* Christopher Brown &middot; <mailto:io@henrian.com>
* Justin Cope &middot; <mailto:justin.cope@utexas.edu>

### Acknowledgements

* Thanks to [Jason Baldridge](http://www.jasonbaldridge.com/) for some of the smoothing algorithms used in the HMM (Hidden Markov Model) algorithm.
