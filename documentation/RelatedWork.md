# Related work

## Cluster computing

### Variants on data flow 
* [Dryad](http://www.news.cs.nyu.edu/~jinyang/sp07/papers/dryad.pdf)

* [Paralex](https://www.cs.utexas.edu/~lorenzo/papers/paralex.pdf)

* [P-RIO](https://drive.google.com/file/d/0B1ytrVudfgrLY3d3cW5sOHRkRkk/view)

* [LGD2F](https://drive.google.com/file/d/0B1ytrVudfgrLdUs1SFBEQjVSWlk/view)

* [CODE](https://drive.google.com/file/d/0B1ytrVudfgrLeUhEQUZSbVpKMVE/view)

### Message passing
* [Cloud Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf)

## Hardware

### Layout
* [Wired: Wire Aware Circuit Design](http://www.cse.chalmers.se/~emax/documents/Wired_CHARME05.pdf)

* [Ruby](https://www.doc.ic.ac.uk/~wl/teachlocal/cuscomp/notes/introRuby.pdf)

* [Lava](http://www.cse.chalmers.se/edu/year/2012/course/_courses_2011/TDA956/Papers/Lava98.pdf)

## GPU computing

### DSLs

* [Obsidian: GPU Programming in Haskell](http://www.cse.chalmers.se/~joels/writing/dccpaper_obsidian.pdf)

## Programming Languages

### NESL
* [NESL: A Nested Data-Parallel Language](https://www.cs.cmu.edu/~guyb/papers/Nesl2.6.pdf)

* [Programming Parallel Algorithms](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.42.4869&rep=rep1&type=pdf)

### Legion
* [Legion: Expressing Locality and Independence with Logical Regions](http://legion.stanford.edu/pdfs/sc2012.pdf)
    In Legion's programming model computations are associated with
    logical (memory) regions. The association of computations to
    regions governs the mapping of computations to hardware at runtime.
    A place to improve on the programming model of Legion is allowing
    more explicit control over what hardware resources are used by a
    computation, as this would allow explicit management of where the logical
    regions reside.

### Chapel

## Misc (also knows as, "stuff I wasn't sure where to put!")

* [Programming Heterogeneous Systems at Exascale](https://drive.google.com/open?id=0B1ytrVudfgrLVGJhamZZZ1JQdWNzOWk0ZzRWZGR2REx0akdN)

* [A Distributed Haskell for the Modern Web](http://haste-lang.org/pubs/haste-licentiate.pdf)
    Anton's lic thesis. This document is interesting because
    of Haske's way of enabling the programming to play
    with running code either on the client or the server
    side of the web application. A limitation of the programming
    model, in our context, is that the implementation of Haste
    favours a master-slave relationship between the client
    and the server, this is naturally due to the fact that
    the primary focus of the programming model is about
    IO-heavy "user experience" kind of applications,
    not HPC.
