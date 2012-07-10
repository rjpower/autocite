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
 2: optional i64 id = 0,
 3: string xml,
 4: list<Citation> incoming,
 5: list<Citation> outgoing,
 6: string text,
 7: string url,
 8: optional i32 year = 0,
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

typedef map<string, double> BagOfWords 
struct LearnerCache {  
   1: map<i64, Document> docs,
   2: map<i64, BagOfWords> bow,
   3: map<i64, BagOfWords> normalized,
   4: BagOfWords idf,
   5: map<i64, list<i64>> matches
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

