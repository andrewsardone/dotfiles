# jobs

Here I can declare scheduled jobs (e.g., via cron) that should be installed on
my personal machine.

## Installation

To get help, run `make help`.

To install the jobs, run `make install`.

## Setup

### cron

Create `*.cron` files in the `jobs` directory that represent a single cron entry. For example:

```cron
# example.cron
*/5 * * * * echo "hello world"
```
