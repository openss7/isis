/*   $RCSfile: pmklst.c,v $ $Revision: 2.1 $ $Date: 90/07/31 16:18:32 $ */
/*   doubly linked list handler
 
     this module manages doubly linked lists headed by a list_type
     and built out of list_nodes.  dynamic allocation routines malloc
     and free are used to manage list_nodes.  The following routines are
     included:
        newlist(*list_type) - initialize list head to empty list.
        enqueue(listypes,*list_type) - place new element listypes at tail
                                      of list *list_type
        enstack(listypes,*list_type) - place new element at head
        listypes dequeue(*list_type) - return head element of list_type
                                      and deallocate
        delete(*list_node,*list_type) - delete node after *list_node
                                      from list and deallocate
        insert(*list_node,*list_node,*list_type) - insert second node after
                                      first node listed in parameters.
                                      first node=nil indicates at head.
        move_node(*list_type,*list_node,*list_node) - move first node to 
                                      position immediately after second node
        copy_list(*list_type,*list_type) - copy one list into another.
        sort_list(*list_type,*int fun) - sort the indicated list based
                                      on the compare function passed.
*/

#include <pmkdat.h>
#include <stdio.h>

void
new_list(list)
	list_type *list;

/* initialize a list to empty.
   preconditions : none
   postconditions : *list is empty */
{
#ifdef dbglst
	printf("init list at %x\n", list);
#endif
	list->head = list->tail = NULL;
}

void
insert(elt1, newelt, list)
	list_node *elt1;
	gen_ptr newelt;
	list_type *list;

/* insert a list element with data newelt after elt1 in list_type.
   if elt1=nil then insert at head
 
   preconditions : list is initialized
   postconditions : elt is at tail of list */
{
	list_node *new_node;

	new_node = (list_node *) malloc(sizeof(list_node));
	new_node->content = newelt;

#ifdef dbglst
	printf("insert node %x ref %x ", new_node, newelt);
#endif

	if (elt1) {		/* previous element not nil so insert after it */
		new_node->next = elt1->next;
		elt1->next = new_node;
		new_node->last = elt1;

#ifdef dbglst
		printf("after %x ", elt1);
#endif

	} else {		/* previous element nil so insert after head. */
		new_node->next = list->head;
		list->head = new_node;
		new_node->last = NULL;

#ifdef dbglst
		printf("at head ");
#endif

	}

	if (!new_node->next)	/* nil next pointer here indicates end of list. update tail. */
		list->tail = new_node;
	else
		(new_node->next)->last = new_node;
	/* otherwise update back ptr. */

#ifdef dbglst
	printf(" next is %x, in %x \n", new_node->next, list);
#endif
}

void
enqueue(elt, list)
	gen_ptr elt;
	list_type *list;

/* insert a list element with data elt at the tail of list_type.
   preconditions : list is initialized
   postconditions : elt is at tail of list */
{
	insert(list->tail, elt, list);
}

void
copy_list(p_src, p_dest)
	list_type *p_src;
	list_type *p_dest;
{
	list_node *cur_node;

	new_list(p_dest);
	for (cur_node = p_src->head; cur_node; cur_node = cur_node->next)
		enqueue(cur_node->content, p_dest);
}

void
enstack(elt, list)
	gen_ptr elt;
	list_type *list;

/* insert a list element with data elt at the head of list_type.
   preconditions : list is initialized
   postconditions : elt is at head of list */
{
	insert(NULL, elt, list);
}

void
delete(delnode, list)
	list_node *delnode;
	list_type *list;

/* delete node *delnode from *list.  
   preconditions : list is initialized and delnode is in it.
   postconditions: *delnode is not in list and has been freed */
{
	list_node *node;

#ifdef dbglst
	printf("delete %x from %x\n", delnode, list);
#endif
	node = (delnode->last);

	if (node) {		/* node not nil - delete node's successor */
		node->next = delnode->next;
		if (node->next)
			(node->next)->last = node;
		else
			list->tail = node;

	} else {		/* node nil - delete head */
		list->head = delnode->next;
		if (list->head)
			list->head->last = NULL;
		else
			list->tail = NULL;
	}
	free(delnode);

}

gen_ptr
dequeue(list)
	list_type *list;

/* dequeue head element from list* and return content thereof.
   return nil if list empty
   preconditions : list initialized
   postconditions : head element of list deleted
  */
{
	gen_ptr elt;			/* remember element to return */

	if (list->head) {
		elt = (list->head)->content;
		delete(list->head, list);
	} else {
		elt = NULL;
	}
	return elt;
}

void
move_node(p_list, p_node, p_new_pred)
	list_type *p_list;
	list_node *p_node, *p_new_pred;

/* move p_node to a position after p_new_pred, or head of list if
   p_new_pred = NULL. 

  preconditions: p_list initialized and p_node and p_new_pred are both
                 currently in p_list
  postconditions: p_node is immediately after p_new_pred in p_list
*/
{
	if (p_node->last != p_new_pred) {	/* check for fortuitous order */
		/* delete p_node from old position */
		if (!(p_node->last)) {	/* p_node is at head */
			if (!(p_list->head = p_node->next))
				p_list->tail = NULL;
			/* p_node also tail */
			else
				(p_node->next)->last = NULL;	/* p_node is head but not tail */
		} else {
			/* p_node is not head */
			if (!((p_node->last)->next = p_node->next))
				p_list->tail = p_node->last;
			/* p_node is tail but not head */
			else
				(p_node->next)->last = p_node->last;
		}
		/* put p_node after p_new_pred */
		if (!(p_node->last = p_new_pred)) {	/* p_node at head */
			p_node->next = p_list->head;
			p_list->head = p_node;
		} else {
			p_node->next = p_new_pred->next;
			p_new_pred->next = p_node;
		}
		if (p_node->next)
			(p_node->next)->last = p_node;	/* p_node not at tail */
		else
			p_list->tail = p_node;
	}
}

void
sort_list(p_list, cmpfun)
	int (*cmpfun) ();
	list_type *p_list;

/* sort the list p_list based on comparisons resulting from calls to
   cmpfun.  cmpfun accepts 2 arguments which are pointers from the content
   fields of 2 list nodes.  cmpfun must return the equivilant of the first
   parameter minus the second for the actual type of node pointed to by
   the list.
   preconditions: p_list is initialized
   postconditions: p_list is sorted in ascending cmpfun order.
   */
{
	register list_node *sorted_to, *move_to, *temp_node;

#ifdef dbglst
	printf("sort_list %x ", p_list);
#endif
	/* scan until list end.  do nothing as long as values ascend. */

	sorted_to = p_list->head;
	while (sorted_to && (sorted_to->next)) {
		if ((*cmpfun) (sorted_to->content, (sorted_to->next)->content) > 0)
			/* found a lower value */
		{
#ifdef dbglst
			printf("B");
#endif
			temp_node = (sorted_to->next);
			/* locate first node smaller than or equal to misplaced node. */
			for (move_to = (sorted_to->last);
			     (move_to) && ((*cmpfun) (move_to->content, temp_node->content) > 0);
			     move_to = (move_to->last)) ;

			move_node(p_list, temp_node, move_to);

		} else {
			sorted_to = sorted_to->next;
#ifdef dbglst
			printf("F");
#endif
		}
	}
#ifdef dbglst
	printf("sorted\n");
#endif
}
