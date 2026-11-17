SYNTHETIC TEST DATA - FOR ACADEMIC DEMONSTRATION ONLY
=====================================================
These CSV files were generated artificially for the ITA14 Q7 assignment.
They are NOT results of scanning any real network or system.

* IP addresses use only documentation ranges reserved by RFC 5737:
  192.0.2.0/24, 198.51.100.0/24, 203.0.113.0/24
* Hostnames use the reserved suffix .example.test
* Scan session IDs start with "SYN-" (synthetic).
* The files intentionally contain DIRTY records so validation and cleaning
  can be demonstrated: missing values, invalid IPs, invalid ports
  (0, -5, 70000, "abc"), invalid protocols/states, bad timestamps,
  exact duplicates, case/whitespace problems and conflicting duplicates.

Files
  hosts.csv                host inventory, session SYN-S1
  services.csv             TCP service scan, session SYN-S1
  udp_scans.csv            UDP scan (includes hosts not in hosts.csv)
  multi_session_scans.csv  sessions SYN-S2 and SYN-S3 (TCP + UDP)
