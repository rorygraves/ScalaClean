package scalaclean.cli;

public enum SourceValidation {
    /**
     * No validation
     */
    NONE,
    /**
     * Skip the file if it doesn't match
     */
    SKIP,
    /**
     * Mark the file as a failed to run rules
     */
    FAIL

}
