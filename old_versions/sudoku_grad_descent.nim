import std/[marshal,streams,times,tables]

var sudokuData:Table[string,tuple[rating:seq[int],forwards_time,backwards_time:Duration;forwards_tries,backwards_tries: int]]

var f = newFileStream("sudokuTable",fmRead)
load(f,sudokuData)
f.close()

#so, tried a weighting based on the binary matrix
#but it wasn't great.
#rating is known to correlate with picking the correct direction
#but it has a variable length. so i don't know what you were thinking could happen there
#so what would this function look like?
#i mean, i'd guess it'd have something to do with left-ness.
#proc likelihood(ratings:seq[int]):float =
#  for i,r in ratings:
#
# in another sense, the ratings function is
# [[0 1 0 0 0 0 0 1 0]     *        [[[3 2 2 1 1 1 1 1 1] [2 3 2 1 1 1 1 1 1]
#  [1 0 0 0 0 1 0 0 0]                [2 1 1 0 0 0 0 0 0] [1 2 1 0 0 0 0 0 0]
#         ...                         [2 1 1 0 0 0 0 0 0] [1 2 1 0 0 0 0 0 0]
#                                     [1 0 0 0 0 0 0 0 0] [0 1 0 0 0 0 0 0 0]
#                                     [1 0 0 0 0 0 0 0 0] [0 1 0 0 0 0 0 0 0]
#                                     [1 0 0 0 0 0 0 0 0] [0 1
#                                     [1                     1
#                                     [1                     1 etc.
#
#                                     err, but flattened i think
#                                     yes, 81x81 matrix
# [[3 2 2 1 1 1 1 1 1 2 1 1 0 0 0 0 0 0 2 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0...]
#  [2 3 2 1 1 1 1 1 1 1 2 1 0 0 0 0 0 0 1 2 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0...]
#  ].T
#  i suppose we can use that
#
#  max(g.g * ratings_matrix) hmm.
#  and what about the interset?
#  [[1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0...]
#   [1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0...]
#   ].T
#   {'1'..'9','.'} - (g.g * interset_matrix).toSet #how fast is toSet?
#   toSet can only loop over that seq. that's testable.
#
#
#
#
#   hmmmm so, multiplying our 81 bytes by a mask is ok and all
#   but surely this is vectorizable
#   what we want at the end is a 9 bit set
#
#   outset <= [ 0 0 0 1 3 0 0 9 ] x [ 0b0001001 ] = 0b100000001
#   this first array might have duplicates are we swizzling?
#   right. [ 0 0 0 1 3 0 0 9] => 0b100000101
#          [ 0 3 0 0 3 0 0 0] => 0b000000100
#
#   clang seems to vectorize just fine
