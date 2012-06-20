namespace java autocite
namespace scala autocite

struct Author {
  1: list<string> name,
}

struct Citation {
 1: string title,
 2: list<Author> authors,
 3: string conference,
}

struct Document {
 1: string title,
 2: string xml,
 3: list<Citation> incoming,
 4: list<Citation> outgoing,
 5: string text
}

struct SearchResult {
  1: string title,
  2: double score,
  3: i32 citations,
  4: string shard,
  5: i64 docid,
  6: i32 luceneDocid
}

struct SearchResults {
 1: list<SearchResult> results,
}

struct LookupResult {
  1: optional Document doc,
}

struct NaiveBayesModel {
  1: i64 docid,
  2: map<string, map<string, double> > classes
}

// Used during indexing to invert citations.
struct CiteOrDoc {
  1: optional Citation cite,
  2: optional Document doc,
} 

service Search {
  bool loadIndex(1: string dir)
  SearchResults search(1: string text, 2: string scorer)
  LookupResult lookup(1: string shard, 2: i64 docid)
  LookupResult random()
}

service Learning {
  LookupResult lookup(1: string title)
  LookupResult random()
  
  void load(1: string shard)
  void prepare(1: list<string> peers)
  void learn()
}

