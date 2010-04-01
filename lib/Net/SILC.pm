####################################################################
#                                                                  #
# Net::SILC -- Object-oriented Perl interface to a SILC server     #
#                                                                   #
#  SILC.pm: The wrapper factory for the Net::SILC modules            #
#                                                                     #
#                                                                      #
#    Copyleft (c) 2004 Derek Laventure spiderman at tranzform dotte ca  #
#                       and Ken Chase math at sizone dotte org         #
#                       All rights reserved.                          #
#                                                                    #
#                                                                   #
#      This module is free software; you can redistribute or       #
#      modify it under the terms of Perl's Artistic License.       #
#                                                                  #
####################################################################


package Net::SILC;  # Test 1: use_ok

use Net::SILC::Client;
use Net::SILC::Event;
use IO::Select;
use Carp;
use strict;
use warnings;
use Devel::Peek;
use Term::ReadKey;
ReadMode 'cbreak';

our $VERSION = '0.03';

# basically this module will provide add_handler and add_global_handler
# methods akin to Net::IRC, which will connect to a set of generic SILC::Client
# callbacks in this module which translate the simple SILC::Client callback
# protocol into Net::SILC::Event's, and then pass them onto the registered
# handlers..

# Ye Olde Contructor Methode. You know the drill.
# Takes absolutely no args whatsoever.
sub new {
    my $proto = shift;

    my $self = {
                '_conn'     => [],
                '_connhash' => {},
		'_clients'  => [],
                '_error'    => IO::Select->new(),
                '_debug'    => 0,
                '_queue'    => {},
                '_qid'      => 'a',
                '_read'     => IO::Select->new(),
                '_timeout'  => 1,
                '_write'    => IO::Select->new(),
            };

    bless $self, $proto;
    return $self;
}

# Creates and returns a new Client object.
# Any args here get passed to Client->connect().
# you need to pass these args: callbacks, user, and server
sub newconn {
    my $self = shift;
    my $user;		# will be hashref to connect settings

    if (ref($_[0])) {
      $user = $_[0];            # SILC style hashref to connection details

    } else {                    # IRC connection details, a reg hash
      my %irc = @_;             # to hash cast, how nast.
      $user = { username => $irc{Nick},
                realname => $irc{Ircname},
                hostname => $irc{LocalAddr},
                server   => $irc{Server},
                keyfile  => $irc{Keyfile} || "mykey.pub",
		immed_connect => $irc{ImmedConnect},
              };
    }

    # the silc-toolkit callbacks are now provided by Client.pm itself
    # it will call any registered 'connected' handler the first time
    # run or run_once is called

    printf("Net::SILC: creating new SILC::Client object..\n");
    my $conn = new Net::SILC::Client($self,$user); # must include key info at this point
    print "Net::SILC: created. connecting to server '", $user->{server}, "'\n";

    print " SILC::newconn(): connecting...\n";
    $conn->connect($user->{server});
    print " SILC::newconn(): connected.\n";
    
    # return the connection to the caller
    return $conn;
}

sub do_one_loop {
  # call silc_client_run_one somehow, basically ;p

# the original Net::IRC code
    my $self = shift;

    my ($ev, $sock, $time, $nexttimer, $timeout);

    # Check the queue for scheduled events to run.

    $time = time();             # no use calling time() all the time.
    $nexttimer = 0;

    # this loop is somewhat cryptic, but basically this deals with the events
    # queued up by Client::schedule($time,$code)
    foreach $ev ($self->queue) {
        if ($self->{_queue}->{$ev}->[0] <= $time) {      # the time the event should happen

            $self->{_queue}->{$ev}->[1]->		  # the coderef (sub) to run
                (@{$self->{_queue}->{$ev}}[2..$#{$self->{_queue}->{$ev}}]); # args

            delete $self->{_queue}->{$ev};

        } else {
            $nexttimer = $self->{_queue}->{$ev}->[0]
                if ($self->{_queue}->{$ev}->[0] < $nexttimer
                    or not $nexttimer);
        }
    }

    # Block until input arrives, then hand the filehandle over to the
    # user-supplied coderef. Look! It's a freezer full of government cheese!

    # for silc connections, we can simply call run_once on each of the
    # client objects..

    # just add a loop_thru_clients() subroutine that calls silc_client_run_one
    # on each Client.pm object, and call that here.. then do a modified loop
    # through the filehandles..

    $self->loop_thru_clients();

    # eventually, i want this stuff to work, too ;p --derek
    # have confidence, my son. it DOES work! --math
    if ($nexttimer) {
        $timeout = $nexttimer - $time < $self->{_timeout}
          ? $nexttimer - $time : $self->{_timeout};
    } else {
        $timeout = $self->{_timeout};
    }

    $timeout = 0.05 if ($timeout == 1);	# default is 1s, needs faster cuz
					# of delay in inline C anyway of 1s

    foreach $ev (IO::Select->select($self->{_read},
                                    $self->{_write},
                                    $self->{_error},
                                    $timeout)) {
        foreach $sock (@{$ev}) {
            my $conn = $self->{_connhash}->{$sock};

            # $conn->[0] is a code reference to a handler sub.
            # $conn->[1] is optionally an object which the
            #    handler sub may be a method of.
            $conn->[0]->($conn->[1] ? ($conn->[1], $sock) : $sock);
        }
    }
}

sub loop_thru_clients {
  my ($self) = @_;
  my $client;
  
  foreach $client (@{$self->{_clients}}) {
    $client->run_once();
  }
}

# Begin the main loop. Wheee. Hope you remembered to set up your handlers
# first... (takes no args, of course)
sub start {
    my $self = shift;

    while (1) {
        $self->do_one_loop();
    }
}

# stolen directly from Net::IRC in case of transferred usefulness ;)

# this should add a Client object to the factory's list of connections
# to run thru with loop_thru_clients
sub addconn {
  my ($self, $client) = @_;
  push(@{$self->{_clients}},$client);
}

# Adds a filehandle to the select loop. Tasty and flavorful.
# Takes 3 args:  a filehandle or socket to add
#                a coderef (can be undef) to pass the ready filehandle to for
#                  user-specified reading/writing/error handling.
#    (optional)  a string with r/w/e flags, similar to C's fopen() syntax,
#                  except that you can combine flags (i.e., "rw").
#    (optional)  an object that the coderef is a method of
sub addfh {
    my ($self, $fh, $code, $flag, $obj) = @_;
    my ($letter);

    die "Not enough arguments to IRC->addfh()" unless defined $code;

    if ($flag) {
        foreach $letter (split(//, lc $flag)) {
            if ($letter eq 'r') {
                $self->{_read}->add( $fh );
            } elsif ($letter eq 'w') {
                $self->{_write}->add( $fh );
            } elsif ($letter eq 'e') {
                $self->{_error}->add( $fh );
            }
        }
    } else {
        $self->{_read}->add( $fh );
    }

    $self->{_connhash}->{$fh} = [ $code, $obj ];
}

# Given a filehandle, removes it from all select lists. You get the picture.
sub removefh {
    my ($self, $fh) = @_;

    $self->{_read}->remove( $fh );
    $self->{_write}->remove( $fh );
    $self->{_error}->remove( $fh );
    delete $self->{_connhash}->{$fh};
}

# Sets or returns the debugging flag for this object.
# Takes 1 optional arg: a new boolean value for the flag.
sub debug {
    my $self = shift;

    if (@_) {
        $self->{_debug} = $_[0];
    }
    return $self->{_debug};
}

# Returns a list of listrefs to event scheduled to be run.
# Takes the args passed to it by Client->schedule()... see it for details.
sub queue {
    my $self = shift;

    if (@_) {
        $self->{_qid} = 'a' if $self->{_qid} eq 'zzzzzzzz';
        my $id = $self->{_qid};
        $self->{_queue}->{$self->{_qid}++} = [ @_ ];
        return ($id);

    } else {
        return keys %{$self->{_queue}};
    }
}

# Takes a scheduled event ID to remove from the queue.
# Returns the deleted coderef, if you actually care.
sub dequeue {
    my ($self, $id) = @_;
    delete $self->{_queue}->{$id}
}

1;
