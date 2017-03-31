#include <po_hi_gqueue.h>
#include <request.h>
#include <stdio.h>
#include <deployment.h>

void ping() {
	__po_hi_request_t request;
	printf("ping\n");
	__po_hi_gqueue_store_out(pingpong_ping1_k, ping1_local_output_ev, &request);
}

void pong(__po_hi_task_id self) {
	printf("pong: %i\n", self);
}
