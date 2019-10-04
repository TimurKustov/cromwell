package cromwell.backend.google.pipelines.common.authentication

import cromwell.cloudsupport.auth.GoogleAuthMode

case class PipelinesApiAuths(genomics: GoogleAuthMode, gcs: GoogleAuthMode)
