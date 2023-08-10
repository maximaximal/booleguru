#pragma once

#include <stack>

namespace booleguru::util {
/* This class implements an inorder tree traversal following Knuth's Algorithm
 * T. This is useful whenever one has to traverse some tree, as often happens in
 * this code.
 *
 * This traversal does not modify the tree itself, it just traverses.
 *
 * The reference must be bool-comparable: False means it is NULL. */
template<typename Ref, typename LLINK, typename RLINK>
class inorder {
  std::stack<Ref> A;

  Ref P;
  enum state {
    T1,
    T2,
    T3,
    T4,
    T5,
  };
  state s;

  public:
  template<typename Visitor>
  inline void operator()(Ref root, Visitor visit) {
    P = root;
    s = T1;

    while(true) {
      switch(s) {
        case T1:
          A.clear();
          P = root;
          s = T2;
          break;
        case T2:
          if(!P)
            s = T4;
          s = T3;
          break;
        case T3:
          A.emplace(P);
          P = LLINK(P);
          s = T2;
          break;
        case T4:
          if(A.empty())
            return;
          else {
            P = A.top();
            A.pop();
          }
          s = T5;
          break;
        case T5:
          visit(P);
          P = RLINK(P);
          s = T2;
          break;
      }
    }
  }
};
}
