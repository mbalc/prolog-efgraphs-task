%%% Set operations

setAdd(set(S), P, set([P | S])) :- \+ member(P, S).
setAdd(set(S), P, set(S)) :- member(P, S).

setAddAll(set(S), set(L), set(X)) :- setAddAll(set(S), L, set(X)).
setAddAll(set(S), [P | L], set(X)) :- setAdd(set(S), P, set(SS)), setAddAll(set(SS), L, set(X)).
setAddAll(set(S), [], set(S)).

setMember(P, set(S)) :- member(P, S).

setRemove(set(S), P, set(X)) :- member(P, S), delete(S, P, X).

listToSet([], set(_)).
listToSet(L, set(S)) :- setAddAll(set([]), L, set(S)).

setsEqual(set([]), set([])).
setsEqual(set([P | S]), set(SS)) :-
  setRemove(set(SS), P, set(NewSS)),
  setsEqual(set(S), set(NewSS)).


%%% jestEFGrafem

% jestEFGrafem(GraphRep)
jestEFGrafem(GraphRep) :-
  hasNoNodeRepetitions(GraphRep),
  areFEdgesValid(GraphRep),
  areEEdgesValid(GraphRep).


% hasNoNodeRepetitions(GraphRep, set(EncounteredNodes))
% True iff GraphRep doesn't have multiple descriptions for
% one node and doesn't have a description of a node
% that is already in EncounteredNodes
hasNoNodeRepetitions(GraphRep) :- hasNoNodeRepetitions(GraphRep, set([])).
hasNoNodeRepetitions([], set(_)).
hasNoNodeRepetitions([node(N, _, _) | Ns], set(EncounteredNodes)) :-
  \+ member(N, EncounteredNodes),
  setAdd(set(EncounteredNodes), N, set(NewEncountereds)),
  hasNoNodeRepetitions(Ns, set(NewEncountereds)).

% areFEdgesValid(GraphRep)
% True iff conditions for F edges are met in GraphRep representation
areFEdgesValid(GraphRep) :-
  fRelationEquivalent(GraphRep).

areEEdgesValid(GraphRep) :-
  eEdgesCorrespondToRealNodes(GraphRep).

% fRelationEquivalent(GraphRep, ExpectedEdges) :-
% Checks whether edge (a, b) is corresponded with a (b, a) edge
% True iff GraphRep has F-edges that are symmetric to
% either themselves or some other F-edge in GraphRep
fRelationEquivalent(GraphRep) :- fEdges(GraphRep, set(FEdges)), !, edgeRelationSymmetric(FEdges).

% edgeRelationSymmetric(Edges)
edgeRelationSymmetric([]).
edgeRelationSymmetric([edge(P, P) | Es]) :-
  edgeRelationSymmetric(Es).
edgeRelationSymmetric([edge(P, Q) | Es]) :-
  member(edge(Q, P), Es),
  delete(Es, edge(Q, P), NewEs),
  edgeRelationSymmetric(NewEs).

% fEdges(GraphRep, set(Edges))
fEdges(Ns, set(Res)) :-
  fEdges(Ns, set([]), set(Res)).
fEdges([node(_, _, []) | Ns], set(Es), set(Res)) :-
  fEdges(Ns, set(Es), set(Res)).
fEdges([node(N, _, [F | Fs]) | Ns], set(Es), set(Res)) :-
  setAdd(set(Es), edge(N, F), set(NewEs)),
  fEdges([node(N, _, Fs) | Ns], set(NewEs), set(Res)).
fEdges([], set(Es), set(Res)) :- Es = Res.

eEdgesCorrespondToRealNodes(GraphRep) :-
  graphNodes(GraphRep, set(Nodes)),
  !,
  eEdgesCorrespondToNodes(GraphRep, Nodes).

% eEdgesCorrespondToNodes(GraphRep, Nodes).
eEdgesCorrespondToNodes([], _).
eEdgesCorrespondToNodes([node(_, [], _) | Ns], Nodes) :-
  eEdgesCorrespondToNodes(Ns, Nodes).
eEdgesCorrespondToNodes([node(_, [E | Es], _) | Ns], Nodes) :-
  member(E, Nodes),
  eEdgesCorrespondToNodes([node(_, Es, _) | Ns], Nodes).

% graphNodes(GraphRep, set(FoundNodes))
% True if Nodes are a set of nodes that are represented in GraphRep
% assumes that GraphRep has no representation repetitions
graphNodes(Ns, set(Res)) :- graphNodes(Ns, set([]), set(Res)).
graphNodes([node(N, _, _) | Ns], set(FoundNodes), set(Res)) :-
  setAdd(set(FoundNodes), N, set(NewFounds)),
  graphNodes(Ns, set(NewFounds), set(Res)).
graphNodes([], set(FoundNodes), set(Res)) :- FoundNodes = Res.


%%% jestDobrzeUlozony(+EFgraf)
jestDobrzeUlozony(EFgraf) :-
  jestEFGrafem(EFgraf),
  fNeighbourhoodLimit(EFgraf, 3),
  isEntrance(EFgraf, Entrance),
  isExit(EFgraf, Exit),
  hasCoveragePath(EFgraf, Entrance, Exit).

% fNeighbourhoodLimit(GraphRep, Limit)
% True iff each node in GraphRep has no more than Limit F-graph neighbours
% Assumes GraphRep F relation is equivalent
fNeighbourhoodLimit([], Limit) :- Limit >= 0.
fNeighbourhoodLimit([node(_, _, FNeighbours) | Ns], Limit) :-
  listToSet(FNeighbours, set(S)),
  length(S, N),
  N < Limit,
  !,
  fNeighbourhoodLimit(Ns, Limit).

% isEntrance(GraphRep, Entrance) :-
% True if the node labeled with Entrance doesn't have any E-edge sources
isEntrance([], _).
isEntrance([node(_, Es, _) | Ns], Entrance) :- \+ member(Entrance, Es), isEntrance(Ns, Entrance).

% isExit(GraphRep, Exit) :-
% True if the node labeled with Exit doesn't have any E-edge destinations
isExit([node(N, [], _) | _], N).
isExit([node(_, _, _) | Ns], Exit) :- isExit(Ns, Exit).

hasCoveragePath(EFgraf, Entrance, Exit). %% TODO - an idea is to first merge all
                                         % strongly connected components with a dfs - then,
                                         % if a dfs is capable of reaching exit from entrance,
                                         % then the predicate is true

jestDobrzePermutujacy(EFgraf).


% getNodeDesc(GraphRep, NodeDesc) :-
getNodeDesc([Desc | _], Desc).
getNodeDesc([_ | Ns], Desc) :- getNodeDesc(Ns, Desc).


jestSucc(EFgraf, Lista1, Lista2) :-
  jestDobrzePermutujacy(EFgraf),
  isFPath(EFgraf, Lista1),
  isFPath(EFgraf, Lista2),
  length(Lista1, M),
  length(Lista2, N),
  N >= M,
  fPathsCorrespondInEGraph(EFgraf, Lista1, Lista2).

%isFPath(GraphRep, NodeList)
isFPath(_, []).
isFPath(GraphRep, [P, Q | Ns]) :-
  getNodeDesc(GraphRep, node(P, _, Fs)),
  member(Q, Fs),
  isFPath(GraphRep, [Q | Ns]).
isFPath(GraphRep, [P]) :-
  getNodeDesc(GraphRep, node(P, _, _)).

% fPathsCorrespondInEGraph(GraphRep, Path1, Path2)
fPathsCorrespondInEGraph(_, [], _).
fPathsCorrespondInEGraph(GraphRep, [M | P1s], [N | P2s]) :-
  getNodeDesc(GraphRep, node(M, Es, _)),
  member(N, Es),
  fPathsCorrespondInEGraph(GraphRep, P1s, P2s).
