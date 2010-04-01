use 5.008003;
use Inline::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Net::SILC',
    ABSTRACT	      => 'Perl interface to the Silc Client library',
    VERSION_FROM      => 'lib/Net/SILC.pm', # finds $VERSION
    OPTIMIZE	      => '-g',
    LDDLFLAGS	      => '-L/usr/local/silc/lib -lsilc -ldl -lpthreads -lsilcclient',
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005

    # this should probably use SILC.pm eventually..
      (ABSTRACT_FROM  => 'lib/Net/SILC/Client.pm', # retrieve abstract from module
       AUTHOR         => 'Derek Laventure <spiderman at tranzform dot ca>') : ()),
);

### from net-irc
#              'NAME'  => 'Net::IRC',
#              'PM'    =>  {
#                  'IRC.pm'        => '$(INST_LIBDIR)/IRC.pm',
#                  'Connection.pm' => '$(INST_LIBDIR)/IRC/Connection.pm',
#                  'Event.pm'      => '$(INST_LIBDIR)/IRC/Event.pm',
#                  'DCC.pm'        => '$(INST_LIBDIR)/IRC/DCC.pm',
#              },
#              'PREREQ_PM' => {
#                  'IO::Select'    => 0,
#                  'Carp'          => 0,
#                  'Socket'        => 0,
#                  'IO::File'      => 0,
#                  'IO::Socket'    => 0,
#                  'Sys::Hostname' => 0,
#              },
#              'VERSION_FROM' => 'IRC.pm', # finds $VERSION
#              'dist' => { 'COMPRESS' => 'gzip --best' },
#
