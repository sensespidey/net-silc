# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl SILC-Client.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 8;
BEGIN { use_ok('Net::SILC::Client') };
# should be a second test here for existence of silc-toolkit.. (before use_ok)

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

# Test 2: test for existence of silc-toolkit
# above, in begin block

my $user = { username => "example", 
	  realname => "example user", 
	  hostname => "example.org",
	  keyfile => "mykey.pub"
	}; # mykey.prv is assumed

my $callbacks = { silc_say             => \&say,
	          silc_channel_message => \&channel_message,
	          silc_private_message => \&private_message,
	          silc_notify          => \&notify,
	          silc_command         => \&command,
	          silc_command_reply   => \&command_reply,
	          silc_connected       => \&connected,
	          silc_disconnected    => \&disconnected,
	          silc_failure         => \&failure,
	          silc_key_agreement   => \&key_agreement,
	          silc_ftp             => \&ftp,
	          silc_detach          => \&detach,
             };

# Test 3: successful creation of client
$client = new SILC::Client($user,$callbacks);
isa_ok($client,"SILC::Client","successful creation of clien");

my $server = "silc.icore.at";
$client->connect($server); # will cause the 'connected' callback to be triggered

# Test 4: connect_to_server
sub connected {
  global $client;
  my($self) = @_;
  is($self,$client,"connect_to_server"); 
}
# Test 5: join a channel
# Test 6: set nick ??
# Test 7: send a command to a server
# Test 8: send a message to a channel
# Test 9: send a message to a person (ourself)

