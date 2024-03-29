# Check for non-standard modules used by this module.
use ExtUtils::MakeMaker;

$DISTNAME = "HTML_CalendarMonth";
$NAME = "HTML::CalendarMonth";

$| = 1;
$missing_modules = 0;

print "Checking for HTML::ElementTable..";
eval {
    require HTML::ElementTable;
};
if ($@) {
    print " failed\n";
    $missing_modules++;
    print <<EOT;

$@
HTML::ElementTable is an enhanced HTML::Element from which HTML::CalendarMonth
inherits.

EOT
    sleep(2);  # Don't hurry too much
} else {
    print " ok\n";
}

print "Checking for Date::Calc..";
eval {
    require Date::Calc;
};
if ($@) {
    print " failed\n";
    print <<EOT;

$@
Date::Calc is only needed if you want to use week-of-year numbering,
or if you plan on manipulating dates that exceed the ability of
localtime() (Prior to Jan 1, 1970 or after 2038).
You can obtain this from CPAN/modules/by-module/Date/Date-Calc-x.x.tar.gz

EOT
    sleep(2);  # Don't hurry too much
} else {
    print " ok\n";
}

print <<EOT if $missing_modules;
The missing modules can be obtained from CPAN.  Visit
http://www.perl.com/CPAN/ to find a CPAN site near you.

EOT

# Time to make a makefile

use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
              'DISTNAME'     => $DISTNAME,
              'NAME'         => $NAME,
	      'VERSION_FROM' => 'lib/HTML/CalendarMonth.pm',
	      'dist'         => {
				  COMPRESS => 'gzip -9f',
				  SUFFIX => 'gz',
				},
	     );
