#pragma once

#include <vector>

namespace booleguru::util {
/* This class implements an inorder tree traversal following Knuth's Algorithm
 * T. This is useful whenever one has to traverse some tree, as often happens in
 * this code.
 *
 * This traversal does not modify the tree itself, it just traverses.
 *
 * The reference must be bool-comparable: False means it is NULL. */
template<typename Ref, typename LLINK_T, typename RLINK_T>
class postorder {
  std::vector<Ref> A;

  Ref P = 0;
  Ref Q = 0;
  enum state {
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
  };
  state s = T1;

  LLINK_T LLINK;
  RLINK_T RLINK;

  public:
  postorder(LLINK_T llink, RLINK_T rlink)
    : LLINK(llink)
    , RLINK(rlink) {}

  template<typename Visitor>
  inline void operator()(Ref root, Visitor visit) {
    P = root;
    s = T1;

    while(true) {
      switch(s) {
        case T1:
          A.clear();
          P = root;
          Q = 0;
          s = T2;
          break;
        case T2:
          if(!P)
            s = T4;
          else
            s = T3;
          break;
        case T3:
          A.emplace_back(P);
          P = LLINK(P);
          s = T2;
          break;
        case T4:
          if(A.empty())
            return;
          else {
            P = A.back();
            A.pop_back();
          }
          s = T5;
          break;
        case T5:
          if(!RLINK(P) || RLINK(P) == Q) {
            s = T6;
          } else {
            A.emplace_back(P);
            P = RLINK(P);
            s = T2;
          }
          break;
        case T6:
          visit(P);
          Q = P;
          s = T4;
          break;
      }
    }
  }
};
}
