#include <Rcpp.h>
#include <cctype>
#include <tuple>
#include <string>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
List reachable_keys(CharacterMatrix maze,
                    int sx,
                    int sy,
                    CharacterVector keys = CharacterVector(0)) {
  // create a deque to store the places in the maze we are iterating over: we
  // store the x, y coordinates then the distance we have travelled
  std::deque <std::tuple <int, int, int>> q;
  // insert the inital position: we need to subtract 1 because C++ uses 0 based
  // indexes
  q.push_back(std::make_tuple(sx-1, sy-1, 0));
  
  // keep track of what positions we have seen, we use a single value to store
  // the coordinates: 1000x + y
  std::unordered_set <int> seen;
  
  // convert the keys set to an unordered set for faster lookups
  std::unordered_set <char> k;
  // insert each of the keys as a single character
  // as String is an Rcpp feature, we need to convert it to a standard c string
  // (get_cstring), then we can get the 1st char from the string ([0]).
  for (String kv : keys) k.insert(kv.get_cstring()[0]);
  
  // declare the return value
  List retval = List::create();
  
  // create a 2d array of the directions that we can move
  int dirs[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0,  1}};
  
  // while there are items in the deque
  while (!q.empty()) {
    // get the first item from the deque
    std::tuple<int, int, int> n = q.front();
    // then remove the first item
    q.pop_front();
    
    // get the x, y coordinates from the tuple and the distance value
    int cx = std::get<0>(n);
    int cy = std::get<1>(n);
    int l  = std::get<2>(n);
    
    // get the character at this position in the maze
    char c = maze(cy, cx)[0];
    
    // if the character is lower case (a key) and isn't in the list of keys
    // already collected, then at this value to the return value
    if (islower(c) && !k.count(c)) {
      retval.push_back(List::create(Named("l") = l,
                                    Named("x") = cx+1,
                                    Named("y") = cy+1,
                                    Named("c") = c));
      // skip to the next item in the deque
      continue;
    }
    
    // iterate over the 4 possible positions
    for (int d = 0; d < 4; d++) {
      // get the x and y coordinates for the next position
      int nx = cx + dirs[d][0];
      int ny = cy + dirs[d][1];
      
      // if we have already visited this location then move to the next item in
      // the deque
      if (seen.count(nx*1000 + ny)) continue; 
      
      // mark this location as seen so we do no revisit it
      seen.insert(nx*1000 + ny);
      
      // get the value at this position
      c = maze(ny, nx)[0];
      
      // if the value is a #, then do nothing
      // if the value is upper case (a door) and we haven't yet visited it, then
      // do nothing
      // otherwise, add this position to the deque to visit properly later
      if (c != '#' && (!isupper(c) || k.count(char(c+32)))) {
        q.push_back(std::make_tuple(nx, ny, l + 1));
      }
    }
  }
  
  // return all of the reachable keys we have found
  return retval;
} 