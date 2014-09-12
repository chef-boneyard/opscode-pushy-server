Crypto Design
=============

Some users of Pushy may prefer that their messages not be sent in cleartext,
so they are free to include sensitive information in the command requests and
replies.  They also may want to ensure that non-authorized clients are
prevented from participating in pushy jobs.  Encrypting the 0mq channel with
an authorized, authenticated key solves both problems.

The zeromq channel between the server and its clients is encrypted by default,
using the built-in encryption of zeromq, "CurveZMQ", a variant of Daniel
Bernstein's CurveCP protocol.  CurveZMQ is a public-key-based encryption
protocol, describing the handshake necessary to establish a secure 0mq
connection.  Once established, the protocol is transparent to the
communicating parties.

CurveZMQ assumes both the client and server have a public/private key-pair,
and each side has access to the other's public key, but it does not define
how the public keys are exchanged.  The Pushy protocol uses a particular
key-exchange mechanism, which does not require persistent storage of any keys.
Instead, both the server and the client generate a key-pair on start-up, and
they exchange keys via REST, before the 0mq socket is established.  The REST
request from the client is signed using the standard Chef signature methods,
guaranteeing that the client is a valid Chef node.

The addition of encryption involves minimal changes to the introductory REST
protocol: the initial GET request from the client contains a single
query-string attribute, "ccpk" ("curve client public key"), and the response
from the server contains a single additional JSON attribute.

Encryption is optional, and may be turned off in the server via an application
configuration variable.  When it is enabled, however, the client must provide
the CCPK, or the config request will be rejected.

When encryption is enabled, but the handshake fails for some reason (e.g. due
to a bug), CurveZMQ fails silently.  Therefore, any protocol on top of CurveZMQ
must include the ability to determine whether the connection was successfully
established.  In pushy, whenever the server sees a new connection, it sends a
message. If the client establishes a connection but does not see a message
within a short time, it assumes the connection has failed, and aborts.

Description
===========

### Connection establishment

The client and server each generate a 0mq public/private key-pair on start-up.
This is a pair of 32-byte values, providing a public-key crypto box built on
top of elliptic curve cryptography (in particular, Curve22519).

The server enables encryption on its 0mq socket, configures it to use the
server private key, then starts listenting.

When the client issues the REST request for its configuration from the pushy
server, it includes its public key as the "ccpk" attribute of the query
string.  The 32-byte binary value is encoded using Z85, the 0mq encoding
algorithm, which transforms every 4 binary bytes into 5 alphanumeric
characters.  (Thus, the resulting string is always exactly 40 characters
long.)

The server requires that the "ccpk" attribute be present, and be a valid Z85
encoding of a 32-byte value.  After validating the REST request (ensuring that
the client, organization, etc are known and authorized to talk the pushy
server, etc), the CCPK is cached in the server.

The server then responds to the client configuration request, including its
own curve public key as an attribute of the JSON object: "curve_public_key".
This is also a Z85-encoding of a 32-byte value.

Upon receipt of the server's public key, the client attempts to establish a
0mq connection to the server, using its own private and public keys, and the
server's public key.  CurveZMQ will use this information during the
handshaking process.

When the server receives the connection request, it validates that the client
public key being sent is one it recognizes.  It does this by verifying the key
is one of those stored in the cache during the REST stage above.

If the connecton request includes a valid public key, then CurveZMQ will
complete the handshake, and the connection will be established.  At that
point, the fact that the connection is encrypted is completely transparent to
both parties.

### Disabled encryption

Encryption can be disabled if the "disable_curve_encryption" variable is set
to true.  In that case the server will not set the encryption flag on the
connection, and it will not require that the config request include the "ccpk"
attribute.  This is intended for backwards-compatibility with clients that do
not implement encryption, and for debugging (so the TCP messages can be
inspected as cleartext).

### CurveZMQ details

The CurveZMQ protocol is documented at <http://curvezmq.org/>.  It is
transparent to the client and server, beyond the need to generate, exchange,
and configure keys.  But understanding the underlying protocol may be helpful
in having confidence in its security.

The gist is that it follows the CurveCP (<http://curvecp.org/>) protocol
closely.  CurveCP is designed by Daniel J. Bernstein, a well-respected
cryptography researcher.  It was designed "to protect every packet against
espionage, corruption, and sabotage".  It has provisions to protect
confidentiality of data (no snooping), integrity (no modification),
and availability (no denial-of-service attacks), among other things.

CurveZMQ includes a few modifications to the protocol, to suit the particular
context of 0mq.  The details are described in
<http://curvezmq.org/page:read-the-docs#toc15>.

Implementation details
======================
The details of the implementation are mostly encapsulated within 0mq, with the
main exception being an implementation of 0mq's ZAP protocol.  It is worth
noting that 0mq's use of Curve22519 crypto-boxes is built on top of libsodium
(<http://doc.libsodium.org/>), which is itself a portable and clear
re-implementation of NaCl (<http://nacl.cr.yp.to/>,
<http://cr.yp.to/highspeed/naclcrypto-20090310.pdf>).  Note that libsodium has
very little documentation, because it implements the same API as NaCl, which
is well-documented.

CurveZMQ (like CurveCP) uses a variety of clever methods to avoid problems
such as man-in-the-middle attacks, key theft attacks, etc.  The techniques
include the use of one-time keys, nonces, etc.  In the end, once the
appropriate session key has been established, it it used to inexpensively
generate an unlimited stream of pesudo-random bytes, which are XORed with the
plaintext before being sent.  (And of course, a similar process happens on the
side doing the decrypting.)  Thus, once the handshake is complete, the amount
of work done to encrypt the ongoing stream is minimal.

### ZAP

0mq provides a configurable plug-in mechanism for authenticating clients, in
the form of ZAP, the "ZeroMQ Authentication Protocol"
(<http://rfc.zeromq.org/spec:27>).  By providing an implementation of a ZAP
handler, a 0mq server may provide its own policy for authenticating clients.
ZAP allows different mechanisms to specify different credentials to be
validated.  In the case of CURVE encryption, the credentials are simply the
32-byte public key of the incoming client.  0mq will provide this key to the
ZAP handler, and expect a response indicating whether the client is allowed to
proceed.

ZAP is implemented by providing a 0mq "inproc" socket, listening at a specific
address: "inproc://zeromq.zap.01".  It receives messages of a particular
structure, including the credentials to be validated, and responds with a
success (a "200" code) or failure (non-"200"), among other attributes.

In the pushy case, the policy is that clients must provide their public key
in a "recent" configuration request.  The implementation of ZAP is provided by
pushy_zap.erl.  After a config's ccpk attribute is validated, it is passed to
pushy_zap:add_key/1, to be stored in a cache (with a 5-second expiration
time).  When the ZAP request comes via the inproc 0mq socket, the incoming
client's key is searched for in the cache; if it exists then pushy_zap
responds with a "200".  The ZAP key must be in the raw 32-byte binary form;
it does not use Z85-encoding at all.

Note that the expiration is short, so any client must follow the REST config
request with the 0mq connection almost immediately, or the connection will be
(silently) rejected due to the failed encryption.

Also note that, if there is a problem talking to the ZAP server, the 0mq
library will simply ignore the encryption completely, and proceed with the
handshake as though the intent was to have an unencrypted stream.  Since the
presence of encryption is transparent to the client and server, there is no
obvious way to tell whether the stream is actually being encrypted.  For this
reason, it is important to be very careful when changing pushy_zap.erl, and
the TCP stream should occasionally be inspected to determine whether the
data is in fact still being encrypted as desired.  (Ideally, there would be
a regression test for this property, but because such a test is challenging to
write, and could require root priveleges, it was deemed out-of-scope for the
initial implementation.)

### Pushy adaptations

The addition of encryption introduces a new failure case, that of a failed
encryption handshake.  0mq provides no feedback to the client library when the
connection cannot be established; instead there is simply a lack of
communication.  This lack of feedback was presumably intended as an aid in
foiling attackers.

Because encryption failures could happen for various reasons, including bugs,
a slowly-responding server, etc, it is useful to be able to identify in pushy
whether the connection was correctly established or not.  The only way to know
for sure is to observe that an expected message did not arrive.  But this is
only possible if a message is expected in the first place.  Normally, when a
client connects to the pushy server, it always receives an "abort" message,
which is intended to reset its state.  If this abort message does not arrive,
the client can assume there is a problem (although the problem could lie with
either the connection or the server).

But there is one circumstance in which, in the past, there was no message on a
new connection, namely, when the client lost and then re-established a
connection (meaning that both the node name and the incarnation ID hadn't
changed). In that case, the server would already have a process for the
node's pushy_node_state, and it would pick up in whatever state it was already
in.  If encryption failed in this case, the client wouldn't know, because no
message was expected.

Therefore, a new "ack" message was added to the protocol, which is only sent
by the server after a re-connection by an existing client.  The client ignores
the message, but by receiving the message, it can be sure that the encrypted
connection is actually working.

The client now notes the time when establishing a connection.  If there are no
messages within 3 seconds, it assumes there's a problem, and logs a message:

    "No messages being received on command port.  Possible encryption problem?"

### Timing during server start-up

Because a failed ZAP request is undectable, it is important that the process
implementing the ZAP server be ready to handle requests before any connections
are accepted.  Therefore, the pushy_broker calls pushy_zap's
"wait_for_start/0" function, which will block until pushy_zap has finished its
initialization.  Only when it is complete will pushy_broker start listening on
the server 0mq socket.

### Possible thread-safety issue

At the time of implementation, 0mq did not call libsodium's "sodium_init()"
function.  This is not required, except "to ensure thread safety of all the
functions provided by the library."  Since 0mq is multi-threaded, this was
probably a bug.  It was noted in:
    <http://lists.zeromq.org/pipermail/zeromq-dev/2014-May/026049.html>
and looks to have been fixed in:
    <https://github.com/zeromq/libzmq/commit/8962b7de45e8b9c99f4aa1073e3cb4b5ad17c3ca>
on May 9, 2014.  However, this is not integrated into the version currently
used by pushy.

In general, there have been quite a few changes to 0mq since the 4.0.4 version
of Mar 10, 2014, which is the one pushy currently uses.  It is probably worth
switching to a more recent version.

### Erlzmq implementation notes

0mq is used by the pushy server via the `erlzmq` Erlang interface.  Among
other things, this library is responsible for calling the Z85 encoding and
decoding functions, as well as implementing the interface for configuring a
0mq socket for Curve encryption.

When specifying a private or public key, code calling into the 0mq library can
pass either a 32-byte binary key, or a 40-character Z85-encoded version of the
key.  One unpleasant element of the 0mq library is that, when setting a key,
it is acceptable to pass in a 40-byte buffer.  But when _getting_ the key, one
must pass in a _41_-byte buffer.  The _size_ of the buffer is used to define
which encoding (raw or Z85) is desired.  (The non-obvious requirement to
specify the buffer as 41 bytes caused a lot of head-scratching when this
feature was added, hence this note.)
