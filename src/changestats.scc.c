/*  File src/changestats.scc.c in package tc.ergmterms.
 *
 *  This software is distributed under the GPL-3 license.  It is free
 *  and open source.
 */
#include "changestats.users.h"

D_CHANGESTAT_FN(d_wttriple) { 
  
  // Declarations
  Edge e;
  Vertex tail, head, change, node3;
  int i, old_tn, new_tn, tn, ttriple_count;
  double edgemult;
  
  //Prep: Calculate the global ttriple count. 
  if(1){
    Edge e1, e2;
	int hnottrans;
	 
	/* *** don't forget tail -> head */    
	ttriple_count=0;
	for (tail=1; tail <= N_NODES; tail++) {
	  STEP_THROUGH_OUTEDGES(tail, e1, head) {
	    hnottrans=1;
	    STEP_THROUGH_INEDGES(head, e2, node3) { 
		  if(hnottrans && IS_INEDGE(node3, tail)){ /* tail -> head base forms transitive */
		    //hnottrans=0;
		    ttriple_count++;
		  }
		}
	  }
    }   
  }
  tn = ttriple_count;
  //end prep
  
  // Beginning wttriple change_stat computation
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    tail = TAIL(i); head = HEAD(i);
    edgemult = IS_OUTEDGE(tail, head) ? -1.0 : 1.0;
    change = 0;
	old_tn = tn; // old_tn is tn before toggles
    
	STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through outedges of head */
      change += IS_INEDGE(node3, tail);
    }
    STEP_THROUGH_INEDGES(head, e, node3) {  /* step through inedges of head */
      change += IS_OUTEDGE(node3, tail) + IS_INEDGE(node3, tail);
    }
    
	tn += edgemult * change; //calculate changes after toggle
	
	new_tn = tn; //new_tn is tn after toggle
	
	// Change stat = new_tn^alpha - old_tn^alpha
	CHANGE_STAT[0] += (pow(new_tn, INPUT_PARAM[0]) - pow(old_tn, INPUT_PARAM[0]));
	
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}

D_CHANGESTAT_FN(d_wttriple2) { 
  Edge e;
  Vertex tail, head, change, node3;
  int i, j;
  double tailattr, edgemult;
  
  /* *** don't forget tail -> head */    
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    tail = TAIL(i);
    head = HEAD(i);
    edgemult = IS_OUTEDGE(tail, head) ? -1.0 : 1.0;
    change = 0;
    if(N_INPUT_PARAMS > 2){ /* match on attributes */
      tailattr = INPUT_ATTRIB[tail-1];
      if(tailattr == INPUT_ATTRIB[head-1]) {
        STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through outedges of head */
          if(tailattr == INPUT_ATTRIB[node3-1])
            change += IS_INEDGE(node3, tail);
        }
        STEP_THROUGH_INEDGES(head, e, node3) { /* step through inedges of head */
          if(tailattr == INPUT_ATTRIB[node3-1])
            change += IS_OUTEDGE(node3, tail) + IS_INEDGE(node3, tail);
        }
        if(N_CHANGE_STATS > 1) { /* diff = TRUE; matches must be tabled */
          for (j=0; j<N_CHANGE_STATS; j++){
            if (tailattr == INPUT_PARAM[j])
              CHANGE_STAT[j] += edgemult * change;
          }
        } else { /* diff = FALSE; all matches equivalent */
              CHANGE_STAT[0] += edgemult * change;          
        }
      }
    }else{ /* no attribute matching */
      STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through outedges of head */
        change += IS_INEDGE(node3, tail);
      }
      STEP_THROUGH_INEDGES(head, e, node3) {  /* step through inedges of head */
        change += IS_OUTEDGE(node3, tail) + IS_INEDGE(node3, tail);
      }
      CHANGE_STAT[0] += edgemult * change * pow(INPUT_PARAM[1], INPUT_PARAM[0]) / INPUT_PARAM[1];
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}

D_CHANGESTAT_FN(d_difftransties) { //by Bruce Desmarais
  Edge e, f;
  int i, echange, ochange;
  int L2th, L2tu, L2uh;
  Vertex tail, head, u, v;
  double cumchange;
  double tailattr, headattr;

  CHANGE_STAT[0] = 0.0;

  /* *** don't forget tail -> head */
  FOR_EACH_TOGGLE(i){
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
      tailattr = INPUT_ATTRIB[tail-1];
      headattr = INPUT_ATTRIB[head-1];
      /* no attributes */
            /* step through outedges of head  */
            STEP_THROUGH_OUTEDGES(head, e, u){
              if (IS_OUTEDGE(tail, u) && (tailattr != headattr) && (tailattr != INPUT_ATTRIB[u-1]) ){
                L2tu=ochange;
                /* step through inedges of u */
                STEP_THROUGH_INEDGES(u, f, v){
                  if(IS_OUTEDGE(tail, v) && (tailattr != INPUT_ATTRIB[v-1]) ){
                    L2tu++;
                    if(L2tu>0) {break;}
                  }
                }
                cumchange += (L2tu==0);
              }
            }
      /* step through inedges of head */

      STEP_THROUGH_INEDGES(head, e, u){
        if (IS_OUTEDGE(tail, u) && (tailattr != headattr) && (tailattr != INPUT_ATTRIB[u-1])){
          L2th++;
        }
        if (IS_OUTEDGE(u, tail) && (tailattr != INPUT_ATTRIB[u-1]) && (headattr != INPUT_ATTRIB[u-1])  ){
          L2uh=ochange;
          /* step through outedges of u */
          STEP_THROUGH_OUTEDGES(u, f, v){
            if(IS_OUTEDGE(v, head) && (INPUT_ATTRIB[v-1] != INPUT_ATTRIB[u-1])){
              L2uh++;
              if(L2uh>0) {break;}
            }
          }
          cumchange += (L2uh==0) ;
        }
      }

    cumchange += (L2th>0) ;
    //  Rprintf("L2th %d echange %d cumchange %f tail %d head %d\n", L2th, echange, cumchange,tail,head);
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}

D_CHANGESTAT_FN(d_edgecov.sender.attr){
	double val;
	Vertex tail, head;
	int nrow, noffset;
	int i, edgeflag;
	
	noffset = BIPARTITE;
	if(noffset > 0){
		/*   nrow = (N_NODES)-(long int)(INPUT_PARAM[0]); */
		nrow = noffset;
		}else{
			nrow = (long int)(INPUT_PARAM[0]);
			}
			
	Vertex node_attr[N_NODES];
	for(i = 0; i < N_NODES; i++){
		node_attr[i] = INPUT_PARAM[N_INPUT_PARAMS - N_NODES + i];
	}
	
	/* *** don't forget tail -> head */    
	ZERO_ALL_CHANGESTATS(i);
	FOR_EACH_TOGGLE(i) {
		tail = TAIL(i);
		if(node_attr[tail - 1] == 1){
			edgeflag=IS_OUTEDGE(tail=TAIL(i), head=HEAD(i));
			val = INPUT_ATTRIB[(head-1-noffset)*nrow+(tail-1)];
			CHANGE_STAT[0] += edgeflag ? -val : val;
		}
		TOGGLE_IF_MORE_TO_COME(i);
		}
	UNDO_PREVIOUS_TOGGLES(i);
}