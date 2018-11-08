/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2017                              *
 *  Author: Ulrich Kremer                    *
 *  Student Version                          *
 *********************************************
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"
static void Preop(Instruction *cur, int LocationValue);
static void Remove(Instruction *remov);
static void PreloadAI(Instruction *cur, int LocationValue);
static void Preload(Instruction *cur, int LocationValue);
static void Prestore(Instruction *cur, int LocationValue);

static void Preop(Instruction *cur, int LocationValue){
	if(cur == NULL){
		return;
	}
	
	while(cur != NULL){
		if(cur->opcode == ADD || cur->opcode == SUB || cur->opcode == MUL || cur->opcode == DIV){
			if(cur->field3 == LocationValue){
				cur->critical = 1;
				Preop(cur->prev, cur->field1);
				Preop(cur->prev, cur->field2);
				Preload(cur->prev, cur->field1);
				Preload(cur->prev, cur->field2);
				PreloadAI(cur->prev, cur->field1);
				PreloadAI(cur->prev, cur->field2);
				return;
			}
		}
		cur = cur->prev;
	}
}

static void Remove(Instruction * remov){
	
	Instruction * cur;
	cur = remov;
	
	while(cur != NULL){
		cur = remov->next;
		free(remov);
		remov = cur;
	}
}


static void PreloadAI(Instruction *cur, int LocationValue){
	if(cur == NULL){
		return;
	}
	
	while(cur != NULL){
		if(cur->opcode == LOADAI && cur->field3 == LocationValue){
			cur->critical = 1;
			Prestore(cur->prev, cur->field2);
		}
		cur = cur->prev;
	}
}

static void Preload(Instruction *cur, int LocationValue){
	if(cur == NULL){
		return;
	}
	while(cur != NULL){
		if(cur->opcode == LOADI && cur->field2 == LocationValue){
			cur->critical = 1;
			return;
		}
		cur = cur->prev;
	}
}


static void Prestore(Instruction *cur, int LocationValue){
	
	if(cur == NULL){
		return;
	}
	while(cur != NULL){
		if(cur->opcode == STOREAI && cur->field3 == LocationValue){
			cur->critical = 1;
			Preop(cur->prev, cur->field1);
			Preload(cur->prev, cur->field1);
			PreloadAI(cur->prev, cur->field1);
			return;
		}
		cur = cur->prev;
	}
	
}

int main()
{
	Instruction *head;	
        Instruction *cur;	
        Instruction *tail;	
        int LocationValue;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	
	head->critical = 1;
	cur = LastInstruction(head);
	
	while(cur != NULL){
                        if(cur->prev == NULL){
				cur->critical = 1;
				Remove(cur->next);
				cur->next = NULL;
				break; 
			}
			if(cur->opcode == OUTPUTAI){
				cur->critical = 1;
				Remove(cur->next);
				cur->next = NULL;
				break;
			}
			cur = cur->prev;
			
		}
	
	cur = head;

	while(cur != NULL){
		if(cur->opcode == OUTPUTAI){
			cur->critical = 1;
			LocationValue = cur->field2;
			Prestore(cur->prev, LocationValue);
		}
		cur = cur->next;
	}

	cur = head->next;
	tail = head;
	
	while(cur != NULL){
		if(!cur->critical){
			tail->next = cur->next;
            free(cur);
            cur = tail->next; 
			cur->prev = tail;
			continue;
		}
		cur = cur->next;    
		tail = tail->next;  
	}
	
	if (head) 
	   PrintInstructionList(stdout, head);
	
        while(head != NULL){
          tail = head->next;
          free(head);
          head = tail; 
        }
	return EXIT_SUCCESS;
}
