use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::task::{Context, Poll, Wake, Waker};

use super::{Value, VmError};

// Each pending operation has its own ready bit. All wakeups signal the same
// executor, including wakeups that arrive between its scan and its wait.
#[derive(Default)]
pub(super) struct WakeQueue {
	signalled: Mutex<bool>,
	changed: Condvar,
}

impl WakeQueue {
	pub(super) fn wait(&self) {
		let flag = self.signalled.lock().unwrap();
		let mut flag = self.changed.wait_while(flag, |flag| !*flag).unwrap();
		*flag = false;
	}
}

struct OperationWake {
	ready: AtomicBool,
	queue: Arc<WakeQueue>,
}

impl Wake for OperationWake {
	fn wake(self: Arc<Self>) {
		self.wake_by_ref();
	}

	fn wake_by_ref(self: &Arc<Self>) {
		self.ready.store(true, Ordering::Release);
		*self.queue.signalled.lock().unwrap() = true;
		self.queue.changed.notify_one();
	}
}

pub(super) type OperationResult = Result<Option<Value>, VmError>;

pub(super) struct PendingOperation {
	future: Pin<Box<dyn Future<Output = OperationResult>>>,
	wake: Arc<OperationWake>,
}

impl PendingOperation {
	pub(super) fn new(future: impl Future<Output = OperationResult> + 'static, queue: Arc<WakeQueue>) -> Self {
		Self {
			future: Box::pin(future),
			wake: Arc::new(OperationWake { ready: AtomicBool::new(true), queue }),
		}
	}

	pub(super) fn is_ready(&self) -> bool {
		self.wake.ready.load(Ordering::Acquire)
	}

	pub(super) fn poll(&mut self) -> Poll<OperationResult> {
		if !self.wake.ready.swap(false, Ordering::AcqRel) {
			return Poll::Pending;
		}

		let waker = Waker::from(self.wake.clone());
		self.future.as_mut().poll(&mut Context::from_waker(&waker))
	}
}
