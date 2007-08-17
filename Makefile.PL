use ExtUtils::MakeMaker;

WriteMakefile(
    NAME		=> 'VCS::CMSynergy',
    AUTHOR		=> 'Roderich Schupp <schupp@argumentum.de>',
    VERSION_FROM	=> 'CMSynergy.pm',
    PREREQ_PM		=> {
    #			     'Expect'		=> '1.15',
			     'File::Spec'	=> 0,
			     'File::Temp'	=> 0,
			     'Memoize'		=> 0,
			   },
    dist		=> { 
			     COMPRESS => 'gzip -9f', 
			     SUFFIX => 'gz' 
			   },
);
