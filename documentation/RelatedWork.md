# Related work

## Cluster computing

### Variants on data flow 
* [Dryad](http://www.news.cs.nyu.edu/~jinyang/sp07/papers/dryad.pdf)
    Dryad is a Microsoft project that basically
    defines a set of combinators for constructing
    data flow graphs. The paper seems very fit for supercomputers
    and large clusters. It features a short thing about graph
    refinement (no technical detail though) at runtime to achieve
    better performance etc. The technology seems to be
    based on a scheduler of some sort or another, this is not discussed
    in great detail. In general the paper lacks in technical depth.
    This looks like the most promising data flow like thing for clusters
    I have yet to see.

* [Paralex](https://www.cs.utexas.edu/~lorenzo/papers/paralex.pdf)

* [P-RIO](https://drive.google.com/file/d/0B1ytrVudfgrLY3d3cW5sOHRkRkk/view)

* [LGD2F](https://drive.google.com/file/d/0B1ytrVudfgrLdUs1SFBEQjVSWlk/view)

* [CODE](https://drive.google.com/file/d/0B1ytrVudfgrLeUhEQUZSbVpKMVE/view)

### Message passing
* [Cloud Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf)

### Topology
* [Managing the Topology of Heterogeneous Cluster Nodes with Hardware Locality (hwloc)](https://www.open-mpi.org/papers/hpcs-2014-hwloc/hpcs-2014-hwloc.pdf)

* [ForestGOMP: an efficient OpenMP environment for NUMA architectures](https://hal.inria.fr/inria-00496295/document)
    This paper tries to adress the need for mapping 
    computations to cores in a large cluster based on
    resource affinity. The mapping happens at runtime
    and the core API for specifying affinity of memory
    and other resources is highly imperative in nature.
    By no means a bad paper, and I think that it would
    be useful to take the contributions of this paper
    and move them over to the more declarative world
    of FP. Moreover proper support for MPI/OpenMP/GOMP
    and this in Feldspar would go a long way to making
    it useful outside of an embedded context.

## Hardware

### Layout
* [Wired: Wire Aware Circuit Design](http://www.cse.chalmers.se/~emax/documents/Wired_CHARME05.pdf)

* [Ruby](https://www.doc.ic.ac.uk/~wl/teachlocal/cuscomp/notes/introRuby.pdf)

* [Lava](http://www.cse.chalmers.se/edu/year/2012/course/_courses_2011/TDA956/Papers/Lava98.pdf)

* [Generating fast multiplyer using clever circuits](http://www.cse.chalmers.se/edu/year/2012/course/TDA956/Papers/Mult_FMCAD04.pdf)

* [Finding regularity: describing and analysing circuits that are not quite regular](https://www.semanticscholar.org/paper/Finding-Regularity-Describing-and-Analysing-Sheeran/d40528e697b20d83d4a290bbeaf4088d93bcf75c/pdf)
    My intuition tells me that shadow values should
    somehow be useful in both optimizing and constructing
    data flow or similar types of constructions...

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
    Anton's lic. thesis. This document is interesting because
    of Haske's way of enabling the programming to play
    with running code either on the client or the server
    side of the web application. A limitation of the programming
    model, in our context, is that the implementation of Haste
    favours a master-slave relationship between the client
    and the server, this is naturally due to the fact that
    the primary focus of the programming model is about
    IO-heavy "user experience" kind of applications,
    not HPC.
