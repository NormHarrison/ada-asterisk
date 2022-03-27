###To do list:

## Both

- Allow user to specify buffer size and/or recursion limit of underlying
  `Socket_Channel_Type`.

- Could the `Agnostic_IO` package specific error kinds be replaced with
  Ada provided exceptions, like those in the `IO_Exceptions` package?
  Maybe we could just pass around an exception ID instead?

## AGI related:

- Reading and writing of data should be handled uniquely for the best safety.
  Trying to make a generic way to handle all scenarios between the two
  packages just doesn't work well.

- Opt for handling the results of AGI commands like the AGI library for Go
  by CyCoreSystems (return a structure/record containing everything of
  interest inside). To include a string in a record though, we will have to
  pass it's length via a descriminint.

- Ensure that the signal handlers in the ProcessAGI
  package actually work.
  UPDATE: They do, but should be revised slightly.

- Consider using the "Connected" component of the "Socket_Channel_Type" to
  trigger an exception or return a true/false value when executing an AGI
  command. We don't currently have an easy way to do this though, as when
  inside the body of the "Command" subprograms, we don't have access to
  variant specific components or primitives.

- Check if the `AGI()` dialplan application can actually connect to IPv6
  addresses when using FastAGI. If not, we may just want to remove the
  ability to bind to IPv6 addresses completely.

- Possibly allow the user to specify a read timeout setting
  for the accepted FastAGI connection BEFORE the initial message is read
  (to prevent possible "attacks").


## AMI related:

- The underlying socket inside the socket channel isn't being closed
when certain exceptions occur in the `Login` procedure (specifically
when the credentials aren't correct, e.g. the last two `raise` statements).

- Create more specific exceptions.

- Look into replacing the current way of handling responses with
  [entry families](https://en.wikibooks.org/wiki/Ada_Programming/Tasking#Entry_families).

- Allow users to "name" each client?

- What behavior should we actually use to handle action responses that
  time out? The previous behavior immediantly released the action ID
  that was used back to the client, but still allocated the response in
  the response object if it arrived, only freeing it when the response
  object was reused. The behavior now is to keep the response object
  acquired until the event eventually does arrive. While cleaner,
  it is highly unlikely that this scenario will occur in real life
  (unless the user sets an extremely short time out).

- Strongly consider returning to using task termination handlers, and
  encourage users to use one for the environment task. Either way though,
  users will still need to create an additional package.
  UPDATE: Task termination handlers are currently being used.

- The user can't override more than a single primitive in a program-level
  declaration region after deriving from a tagged type. In our case, this
  creates unnecessary requirements that place an uneeded burden on package
  users. Our solutions essentially consist of:

  1. Find more of a good reason for making package users declare a new
     instance of the type (meaning it must be done inside a package).
     The main advantage to this solution is that it forces users to
     at least acknowledge the need to handle event loop terminations
     at compile time, via means the language specification provides.

  2. Instead of making the `Event_Loop_Termination_Callback` abstract,
     make it `null`, preventing the `Client_Type` itself from having
     to be abstract too (causing the need for derivation).

  3. (Sanest option?) Revert to handling event loop termination via
     Ada's own task termination handlers.

- If the client becomes disconnected, can it's event loop instance
  ever be restarted? Assuming no. In that case, instead of exiting the
  the event loop when logging off, we would need to make it return to a
  rendezvous position (an `accept` statement). This could be done via two
  loops, or (possibly) via a `goto`. While this idea cannot assist
  in the scenario where the event loop terminates due to an exception,
  it really shouldn't, as the exception should be receive proper handling.
  UPDATE: The event loop is now allocated on the heap, and is re-created
  each time `Login` is invoked. After tests, it seems safe to allocate
  task on the heap without any extra actions needed.

- It is possible that the user logs off with unfreed action responses
  waiting in the client's state (ones that arrived after the set timeout).
  Should we manually free any allocated responses logoff during a call
  to `Logoff`? It may be smarter to go back and revise how action responses
  are handled.
