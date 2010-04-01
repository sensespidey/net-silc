#####################################################################
#                                                                   #
#   Net::SILC -- Object-oriented Perl interface to an SILC server    #
#                                                                     #
#      Event.pm: The basic data type for any SILC occurrence.          #
#                                                                       #
#    Copyright (c) 2004 Derek Laventure	spiderman at tranzform dotte ca	#
#			and Ken Chasse math at sizone dotte org        #
#                       All rights reserved.                          #
#		   						     #	
#                                                                   #
#      This module is free software; you can redistribute or        #
#      modify it under the terms of Perl's Artistic License.        #
#                                                                   #
#####################################################################

package Net::SILC::Event;

use strict;
my %_names;

# Constructor method for Net::SILC::Event objects.
# Takes at least 4 args:  the type of event
#                         the person or server that initiated the event
#                         the recipient(s) of the event, as arrayref or scalar
#                         the name of the format string for the event
#            (optional)   any number of arguments provided by the event
sub new {
    my $class = shift;

    # -- #perl was here! --
    #   \mjd: Under the spreading foreach loop, the lexical variable stands.
    #   \mjd: The my is a mighty keyword, with abcessed anal glands.
    #   \mjd: Apologies to Mr. Longfellow.

    my $self = { 'type'   =>  $_[0],
		 'from'   =>  $_[1],
		 'to'     =>  ref($_[2]) eq 'ARRAY'  ?  $_[2]  :  [ $_[2] ],
		 'format' =>  $_[3],
		 'args'   =>  [ @_[4..$#_] ],
	       };
    
    bless $self, $class;
    
    if ($self->{'type'} !~ /\D/) {
		$self->{'type'} = $self->trans($self->{'type'});
    } else {
		$self->{'type'} = lc $self->{'type'};
    }

    #  ChipDude: "Beware the method call, my son!  The subs that grab, the
    #             args that shift!"
    #      \mjd: That's pretty good.

    $self->from($self->{'from'});     # sets nick, user, and host
    $self->args(@{$self->{'args'}});  # strips colons from args
    
    return $self;
}

# Sets or returns an argument list for this event.
# Takes any number of args:  the arguments for the event.
sub args {
    my $self = shift;

    if (@_) {
        my (@q, $i, $ct) = @_;       # This line is solemnly dedicated to \mjd.

        $self->{'args'} = [ ];
        while (@q) {
            $i = shift @q;
            next unless defined $i;

            if ($i =~ /^:/ and $ct) {                # Concatenate :-args.
                 $i = join ' ', (substr($i, 1), @q);
                 push @{$self->{'args'}}, $i;
                 last;
            }
            push @{$self->{'args'}}, $i;
            $ct++;
        }
    }

    return @{$self->{'args'}};
}

# Dumps the contents of an event to STDERR so you can see what's inside.
# Takes no args.
sub dump {
    my ($self, $arg, $counter) = (shift, undef, 0);   # heh heh!

    printf STDERR "TYPE: %-30s    FORMAT: %-30s\n",
        $self->{'type'}, $self->{'format'};
    print STDERR "FROM: ", $self->{'from'}, "\n";
    print STDERR "TO: ", join(", ", @{$self->{'to'}}), "\n";
    foreach $arg (@{$self->{'args'}}) {
		print "Arg ", $counter++, ": ", $arg, "\n";
    }
}

# -- #perl was here! --
# <Meta-tron> lets say I had [snip]<unknown> this is a lot of text <foo> and I
#             would like to grab it. <unknown> [/snip] how would I be able to
#             write a regular expression that matched the stuff between the
#             <unknown>'s, (the actual contenty of <unknown> is really unknown.
# <Meta-tron> god Im a mess...
#      <\mjd> How can you tell the computer to recognize something unknown when
#             you don't know what it is?
#      <\mjd> ``Hi, just watch out for something interesting, OK?''
#    <mendel> with DWIM::Match, of course
#      <\mjd> ``And pick up a beer if you pass by the 7-11.''
    

# Sets or returns the format string for this event.
# Takes 1 optional arg:  the new value for this event's "format" field.
sub format {
    my $self = shift;

    $self->{'format'} = $_[0] if @_;
    return $self->{'format'};
}

# Sets or returns the originator of this event
# Takes 1 optional arg:  the new value for this event's "from" field.
sub from {
    my $self = shift;
    my @part;
    
    if (@_) {
	# avoid certain irritating and spurious warnings from this line...
	{ local $^W;
	  @part = split /[\@!]/, $_[0], 3;
        }
	
	$self->nick(defined $part[0] ? $part[0] : '');
	$self->user(defined $part[1] ? $part[1] : '');
	$self->host(defined $part[2] ? $part[2] : '');
	defined $self->user ?
	    $self->userhost($self->user . '@' . $self->host) :
	    $self->userhost($self->host);
	$self->{'from'} = $_[0];
    }
    return $self->{'from'};
}

# Sets or returns the hostname of this event's initiator
# Takes 1 optional arg:  the new value for this event's "host" field.
sub host {
    my $self = shift;

    $self->{'host'} = $_[0] if @_;
    return $self->{'host'};
}

# Sets or returns the nick of this event's initiator
# Takes 1 optional arg:  the new value for this event's "nick" field.
sub nick {
    my $self = shift;

    $self->{'nick'} = $_[0] if @_;
    return $self->{'nick'};
}

# Sets or returns the recipient list for this event
# Takes any number of args:  this event's list of recipients.
sub to {
    my $self = shift;
    
    $self->{'to'} = [ @_ ] if @_;
    return wantarray ? @{$self->{'to'}} : $self->{'to'};
}

# Simple sub for translating server numerics (or IRC style text) 
# to their appropriate names.
# Takes one arg:  the number/irc event text to be translated.
sub trans {
    shift if (ref($_[0]) || $_[0]) =~ /^Net::SILC/;
    my $ev = shift;		# $ev is INCOMING from silc toolkit.
				# or from registering an event handler
    
    print STDERR "SILC::Event::trans(): translating event '$ev'\n";

    return (exists $_names{$ev} ? $_names{$ev} : undef);
}

# Sets or returns the type of this event
# Takes 1 optional arg:  the new value for this event's "type" field.
sub type {
    my $self = shift;
    
    $self->{'type'} = $_[0] if @_;
    return $self->{'type'};
}

# Sets or returns the username of this event's initiator
# Takes 1 optional arg:  the new value for this event's "user" field.
sub user {
    my $self = shift;

    $self->{'user'} = $_[0] if @_;
    return $self->{'user'};
}

# Just $self->user plus '@' plus $self->host, for convenience.
sub userhost {
    my $self = shift;
    
    $self->{'userhost'} = $_[0] if @_;
    return $self->{'userhost'};
}

### THIS IS THE MAIN THING THAT NEEDS TO CHANGE HERE ###
%_names = (
  # suck!  these aren't treated as strings --
  # 001 ne 1 for the purpose of hash keying, apparently.
  '001' => 'welcome',
  '002' => 'yourhost',
  '003' => 'created',
  '004' => 'myinfo',
  '005' => 'map',     # Undernet Extension, Kajetan@Hinner.com, 17/11/98
  '006' => 'mapmore',     # Undernet Extension, Kajetan@Hinner.com, 17/11/98
  '007' => 'mapend',     # Undernet Extension, Kajetan@Hinner.com, 17/11/98          
  '008' => 'snomask',     # Undernet Extension, Kajetan@Hinner.com, 17/11/98     
  '009' => 'statmemtot',   # Undernet Extension, Kajetan@Hinner.com, 17/11/98     
  '010' => 'statmem',     # Undernet Extension, Kajetan@Hinner.com, 17/11/98     

  #irc stuff here.
  '251' => 'IRC_RPL_LUSERCLIENT',
  '252' => 'IRC_RPL_LUSEROP',
  '253' => 'IRC_RPL_LUSERUNKNOWN',
  '254' => 'IRC_RPL_LUSERCHANNELS',
  '255' => 'IRC_RPL_LUSERME',
  '302' => 'IRC_RPL_USERHOST',
  '375' => 'IRC_RPL_MOTDSTART',
  '372' => 'IRC_RPL_MOTD',
  '461' => 'IRC_passMoreArgsNeeded',
  '409' => 'IRC_noorigin',
  '405' => 'IRC_toomanychannels',  # XXX need do something about this!
  '404' => 'IRC_cannot_send_to_channel',
  '403' => 'IRC_no_such_channel',
  '401' => 'IRC_no_such_server',
  '402' => 'IRC_no_such_nick',
  '407' => 'IRC_too many targets',
  '433' => 'IRC_ERR_NICKNAMEINUSE',
  '436' => 'IRC_nick_collision',
  '465' => 'IRC_ERR_YOUREBANNEDCREEP',
  '353' => 'IRC_RPL_NAMREPLY',

  # irc scripts use this to detect themselves successfully joining a channel.
  # invented a new name for silc ot use, 'selfjoin' instead fo '366' -
  # irc's join is when other's join a chan, differentiates from self joining
  # a chan. silc doenst have this, has only one event, so must get event
  # handler to differentiate for us. --math

  '366' => 'selfjoin',   	# was IRC_RPL_ENDOFNAMES

  # XXX hack alurt! these should be motd, but moved to connected for now...
  #'376' => 'motd', #no connect(ed) in irc, was IRC_RPL_ENDOFMOTD
  #'422' => 'motd', #no connect(ed) in irc, was IRC_nomotd
  '376' => 'connected', #no connect(ed) in irc, was IRC_RPL_ENDOFMOTD
  '422' => 'connected', #no connect(ed) in irc, was IRC_nomotd

  '311' => 'whois',  	# was IRC_whoisuser
  '312' => 'whoisserver',
  '313' => 'whoisoperator',
  '314' => 'whowasuser',
  '315' => 'endofwho',
  '316' => 'whoischanop',
  '317' => 'whoisidle',
  '318' => 'endofwhois',
  '319' => 'whoischannels',


  # irc text event to silc translations here
  'msg'     => 'private',

# 'welcome' => 'connected',	# no CONNECT(ED) event per se in irc
#  'welcome' => 'say',	# XXX HACK! need connected before welcome
);

# THE POD DOCS NEED CHANGING AS WELL #
1;

__END__

=head1 NAME

Net::SILC::Event - A class for passing event data between subroutines

=head1 SYNOPSIS

None yet. These docs are under construction.

=head1 DESCRIPTION

This documentation is a subset of the main Net::SILC documentation. If
you haven't already, please "perldoc Net::SILC" before continuing.

Net::SILC::Event defines a standard interface to the salient information for
just about any event your client may witness on SILC. It's about as close as
we can get in Perl to a struct, with a few extra nifty features thrown in.

=head1 METHOD DESCRIPTIONS

This section is under construction, but hopefully will be finally written up
by the next release. Please see the C<irctest> script and the source for
details about this module.

=head1 LIST OF EVENTS

Net::SILC is an entirely event-based system, which takes some getting used to
at first. To interact with the SILC server, you tell Net::SILC's server
connection to listen for certain events and activate your own subroutines when
they occur. Problem is, this doesn't help you much if you don't know what to
tell it to look for. Below is a list of the possible events you can pass to
Net::SILC, along with brief descriptions of each... hope this helps.

=head2 Common events

=over

=item *

nick

The "nick" event is triggered when the client receives a NICK message, meaning
that someone on a channel with the client has changed eir nickname.

=item *

quit

The "quit" event is triggered upon receipt of a QUIT message, which means that
someone on a channel with the client has disconnected.

=item *

join

The "join" event is triggered upon receipt of a JOIN message, which means that
someone has entered a channel that the client is on.

=item *

part

The "part" event is triggered upon receipt of a PART message, which means that
someone has left a channel that the client is on.

=item *

mode

The "mode" event is triggered upon receipt of a MODE message, which means that
someone on a channel with the client has changed the channel's parameters.

=item *

topic

The "topic" event is triggered upon receipt of a TOPIC message, which means
that someone on a channel with the client has changed the channel's topic.

=item *

kick

The "kick" event is triggered upon receipt of a KICK message, which means that
someone on a channel with the client (or possibly the client itself!) has been
forcibly ejected.

=item *

public

The "public" event is triggered upon receipt of a PRIVMSG message to an entire
channel, which means that someone on a channel with the client has said
something aloud.

=item *

msg

The "msg" event is triggered upon receipt of a PRIVMSG message which is
addressed to one or more clients, which means that someone is sending the
client a private message. (Duh. :-)

=item *

notice

The "notice" event is triggered upon receipt of a NOTICE message, which means
that someone has sent the client a public or private notice. (Is that
sufficiently vague?)

=item *

ping

The "ping" event is triggered upon receipt of a PING message, which means that
the SILC server is querying the client to see if it's alive. Don't confuse this
with CTCP PINGs, explained later.

=item *

other

The "other" event is triggered upon receipt of any number of unclassifiable
miscellaneous messages, but you're not likely to see it often.

=item *

invite

The "invite" event is triggered upon receipt of an INVITE message, which means
that someone is permitting the client's entry into a +i channel.

=item *

kill

The "kill" event is triggered upon receipt of a KILL message, which means that
an SILC operator has just booted your sorry arse offline. Seeya!

=item *

disconnect

The "disconnect" event is triggered when the client loses its
connection to the SILC server it's talking to. Don't confuse it with
the "leaving" event. (See below.)

=item *

leaving

The "leaving" event is triggered just before the client deliberately
closes a connection to an SILC server, in case you want to do anything
special before you sign off.

=item *

umode

The "umode" event is triggered when the client changes its personal mode flags.

=item *

error

The "error" event is triggered when the SILC server complains to you about
anything. Sort of the evil twin to the "other" event, actually.

=back

=head2 CTCP Requests

=over

=item *

cping

The "cping" event is triggered when the client receives a CTCP PING request
from another user. See the irctest script for an example of how to properly
respond to this common request.

=item *

cversion

The "cversion" event is triggered when the client receives a CTCP VERSION
request from another client, asking for version info about its SILC client
program.

=item *

csource

The "csource" event is triggered when the client receives a CTCP SOURCE
request from another client, asking where it can find the source to its
SILC client program.

=item *

ctime

The "ctime" event is triggered when the client receives a CTCP TIME
request from another client, asking for the local time at its end.

=item *

cdcc

The "cdcc" event is triggered when the client receives a DCC request of any
sort from another client, attempting to establish a DCC connection.

=item *

cuserinfo

The "cuserinfo" event is triggered when the client receives a CTCP USERINFO
request from another client, asking for personal information from the client's
user.

=item *

cclientinfo

The "cclientinfo" event is triggered when the client receives a CTCP CLIENTINFO
request from another client, asking for whatever the hell "clientinfo" means.

=item *

cerrmsg

The "cerrmsg" event is triggered when the client receives a CTCP ERRMSG
request from another client, notifying it of a protocol error in a preceding
CTCP communication.

=item *

cfinger

The "cfinger" event is triggered when the client receives a CTCP FINGER
request from another client. How to respond to this should best be left up
to your own moral stance.

=item *

caction

The "caction" event is triggered when the client receives a CTCP ACTION
message from another client. I should hope you're getting the hang of how
Net::SILC handles CTCP requests by now...

=back

=head2 CTCP Responses

=over

=item *

crping

The "crping" event is triggered when the client receives a CTCP PING response
from another user. See the irctest script for an example of how to properly
respond to this common event.

=item *

crversion

The "crversion" event is triggered when the client receives a CTCP VERSION
response from another client.

=item *

crsource

The "crsource" event is triggered when the client receives a CTCP SOURCE
response from another client.

=item *

crtime

The "crtime" event is triggered when the client receives a CTCP TIME
response from another client.

=item *

cruserinfo

The "cruserinfo" event is triggered when the client receives a CTCP USERINFO
response from another client.

=item *

crclientinfo

The "crclientinfo" event is triggered when the client receives a CTCP
CLIENTINFO response from another client.

=item *

crfinger

The "crfinger" event is triggered when the client receives a CTCP FINGER
response from another client. I'm not even going to consider making a joke
about this one.

=back

=head2 Numeric Events

=over

=item *

There's a whole lot of them, and they're well-described elsewhere. Please see
http://silcnet.org/docs/draft-riikonen-silc-pp-08.txt for detailed
descriptions, or the Net::SILC::Event.pm source code for a quick list.

=back

=head1 AUTHORS

Conceived and initially developed by Greg Bacon E<lt>gbacon@adtran.comE<gt> and
Dennis Taylor E<lt>dennis@funkplanet.comE<gt>.

Ideas and large amounts of code donated by Nat "King" Torkington E<lt>gnat@frii.comE<gt>.

Borrowed for use in Net::SILC by Derek Laventure E<lt>spiderman at tranzform dot caE<gt>.

=head1 URL

Up-to-date source and information about the Net::SILC project can be found at
http://silcpm.sourceforge.net/ .

=head1 SEE ALSO

=over

=item *

perl(1).

=item *

http://silcnet.org/

=back

=cut

