pub trait Do {
    /// (Re-) Do this
    fn redo(&mut self);
}

pub trait Undo {
    /// Undo this
    fn undo(&mut self);
}

/// A simple undo chain able to handle `undo` and `redo` operations depending on `T`
///
/// Common terms used:\
/// `done` element - An element which was recently *executed* or otherwise fullfilled its purpose\
/// `do` element - An element which can be *executed*, in general implements the `Do` trait
pub struct UndoChain<T> {
    backward: Vec<T>,
    forward: Vec<T>,
}

impl<T> UndoChain<T> {
    pub fn new() -> Self {
        Self {
            backward: Vec::new(),
            forward: Vec::new(),
        }
    }

    /// Add the given `done` element as the most recently done element.
    pub fn add_done(&mut self, done: T) {
        self.backward.push(done);
    }
}

impl<T: Do> UndoChain<T> {
    /// Indicates whether there is any action available for a `Redo` operation.
    pub fn can_redo(&self) -> bool {
        self.forward.len() > 0
    }

    /// Do the given action and add it as the most recently done activity.
    pub fn do_it(&mut self, mut it: T) {
        it.redo();
        self.backward.push(it);
    }
}

impl<T: Undo> UndoChain<T> {
    /// Indicates whether there is any action available for a `Undo` operation.
    pub fn can_undo(&self) -> bool {
        self.backward.len() > 0
    }
}

impl<T: Do> Do for UndoChain<T> {
    /// Redo the most recent undone activity
    fn redo(&mut self) {
        if let Some(mut el) = self.forward.pop() {
            el.redo();
            self.backward.push(el);
        }
    }
}

impl<T: Undo> Undo for UndoChain<T> {
    /// Undo the most recent activity
    fn undo(&mut self) {
        if let Some(mut el) = self.backward.pop() {
            el.undo();
            self.forward.push(el);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct MockDoAndUndo {
        was_done: bool,
        index: usize,
    }

    impl MockDoAndUndo {
        pub fn new(was_done: bool, index: usize) -> Self {
            Self { was_done, index }
        }
    }

    impl Do for MockDoAndUndo {
        fn redo(&mut self) {
            assert_eq!(self.was_done, false);
            self.was_done = true;
        }
    }

    impl Undo for MockDoAndUndo {
        fn undo(&mut self) {
            assert_eq!(self.was_done, true);
            self.was_done = false;
        }
    }

    #[test]
    fn undo_actually_undos_the_last_activity() {
        let mut chain = UndoChain::new();
        chain.do_it(MockDoAndUndo::new(false, 0));
        chain.do_it(MockDoAndUndo::new(false, 1));
        chain.do_it(MockDoAndUndo::new(false, 2));

        chain.undo();

        let last = &chain.forward.last().expect("Should have one doable item");

        assert_eq!(last.was_done, false);
        assert_eq!(last.index, 2);
    }

    #[test]
    fn redo_redoes_the_last_undone_activity() {
        let mut chain = UndoChain::new();
        chain.do_it(MockDoAndUndo::new(false, 0));
        chain.do_it(MockDoAndUndo::new(false, 1));
        chain.do_it(MockDoAndUndo::new(false, 2));

        chain.undo();
        chain.undo();
        chain.undo();
        chain.redo();

        let last = &chain
            .backward
            .last()
            .expect("Should have one undoable item");

        assert_eq!(chain.forward.len(), 2);
        assert_eq!(last.was_done, true);
        assert_eq!(last.index, 0);
    }
}
