#include "changestats.users.h"

/* absdiffby statistic */
  
CHANGESTAT_FN(d_absdiffby) {
  double change, offset, byval; Vertex t, h; int i;
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    t = TAIL(i); h = HEAD(i);
    byval = INPUT_PARAM[t + N_NODES];
    offset = INPUT_PARAM[0];
    if (byval == 1) {
      change = fabs(INPUT_PARAM[t] - INPUT_PARAM[h] - offset);
    } else {
      change = fabs(INPUT_PARAM[t] - INPUT_PARAM[h] + offset);
    }
    CHANGE_STAT[0] += IS_OUTEDGE(t,h) ? -change : change;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}

