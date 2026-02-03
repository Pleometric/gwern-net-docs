---
sidebar_position: 4
---

# memoriam.sh

**Path:** `nginx/memoriam.sh` | **Language:** Bash

Bash script that generates a daily-rotating `X-Clacks-Overhead` HTTP header to commemorate deceased scientists, writers, and intellectuals.

---

## Overview

`memoriam.sh` is a memorial system that honors notable figures by displaying their names in HTTP response headers on specific dates corresponding to their deaths. The script implements a generalized version of the Terry Pratchett "GNU Terry Pratchett" tribute, which originated from his Discworld novels and became a popular web memorial tradition after his death in 2015.

The script maintains an associative array mapping dates (MM-DD format) to names, selects the appropriate person for the current date, and outputs an nginx configuration directive setting the `X-Clacks-Overhead` header. On days without memorial entries, the script produces no output, and no header is set.

The system runs daily via cron, regenerating `/etc/nginx/conf.d/memoriam.conf` and reloading nginx at 5:00 AM Eastern Time. The memorial configuration is included by the main `gwern.net.conf`, making it a zero-runtime-cost implementation that still provides dynamic daily rotation.

## Overview of X-Clacks-Overhead

The `X-Clacks-Overhead` header originates from Terry Pratchett's Discworld series, specifically the novel *Going Postal*. In the books, the "clacks" is an optical telegraph system using semaphore towers to transmit messages across long distances. Tower operators developed a tradition of keeping the names of deceased operators circulating in the overhead traffic with the prefix "GNU" (an in-universe signal code meaning the message should be passed on, not logged, and turned around at the end of the line—ensuring it travels forever).

After Pratchett's death in 2015, web developers adopted this as a memorial tradition, adding `X-Clacks-Overhead: GNU Terry Pratchett` headers to their sites. Gwern.net generalizes this to rotate through a curated list of influential deceased individuals, creating a "perpetual memorial" in the infrastructure of the web itself.

## Key Functions

### Data Structure

```bash
declare -A memorials=(
    ["01-01"]="Niklaus Wirth"
    ["01-02"]="Derek Parfit"
    ["01-04"]="T. S. Eliot"
    ...
    ["12-28"]="Ian Murdock"
)
```

The memorial database is a Bash associative array mapping MM-DD date strings to names. If you want multiple people on the same date, you must use a comma-separated value (e.g., `["01-14"]="Kurt Gödel, Lewis Carroll"`); `pick_random_name` will then choose one. Duplicate keys overwrite earlier entries.

### Random Name Selection

```bash
pick_random_name() {
    local names_string="${1//, /$'\n'}"
    IFS=$'\n' read -r -d '' -a NAMES <<< "$names_string"
    echo "${NAMES[$RANDOM % ${#NAMES[@]}]}"
}
```

When a date has multiple names, this function:
1. Replaces `", "` with newlines to split the string
2. Reads into an array
3. Randomly selects one using `$RANDOM % array_length`
4. Echoes the selected name

This ensures that on dates like January 14 (Gödel and Carroll), each person is commemorated approximately 50% of the time over the years.

### Date Lookup and Output

```bash
today=$(TZ="America/New_York" date +%m-%d)

if [[ -v memorials[$today] ]]; then
    author=$(pick_random_name "${memorials[$today]}")
    echo "add_header X-Clacks-Overhead \"$author\";"
fi
```

The script:
1. Gets today's date in MM-DD format using US Eastern Time (Gwern's timezone)
2. Checks if an entry exists for today
3. If found, picks a random name only if the value is a comma-separated list; otherwise it outputs the single name
4. If not found, produces no output (empty config file)

### Data Validation

```bash
declare -A value_check=()
declare -A pair_check=()
for key in "${!memorials[@]}"; do
    value="${memorials[$key]}"
    if [[ -n "${value_check[$value]}" ]]; then
        echo "Duplicate value found: $value" >&2
        exit 1
    fi
    # ... (check for unique key-value pairs)
done
```

At startup, the script validates the memorial database to ensure:
- No duplicate values (same person listed multiple times for different dates)
- No duplicate key-value pairs (same person on same date listed twice)

This prevents configuration errors and data entry mistakes.

## Deployment Workflow

### Cron Job

```bash
0 5 * * * rm /etc/nginx/conf.d/memoriam.conf && /home/gwern/gwern.net/static/nginx/memoriam.sh > /etc/nginx/conf.d/memoriam.conf && systemctl reload nginx
```

Daily at 5:00 AM (server time):
1. Remove old memorial config
2. Regenerate by running `memoriam.sh`
3. Reload nginx to apply new header

### Nginx Integration

In `gwern.net.conf`:
```nginx
include /etc/nginx/conf.d/memoriam.conf;
```

The generated single-line config is included in the main server block. When the file is empty (no memorial for that date), the include does nothing. When populated, it adds the header to all responses.

### Example Output

```bash
$ bash memoriam.sh
add_header X-Clacks-Overhead "Claude Shannon";
```

On February 24 (Shannon's death date), the script outputs this directive, which nginx then includes, resulting in:

```
HTTP/1.1 200 OK
...
X-Clacks-Overhead: Claude Shannon
...
```

## Memorial Database

The script commemorates 137 individuals (as of the 2025-11-29 timestamp) across multiple domains:

**Computer Science & Mathematics:**
- Turing, Shannon, von Neumann, Gödel, Dijkstra, Knuth-adjacent figures
- John McCarthy, Alan Perlis, Marvin Minsky, John Conway

**Statistics & Decision Theory:**
- Bayes, Fisher, Pearson, de Finetti, Savage, Kolmogorov

**Philosophy & Literature:**
- Borges, Pratchett, Asimov, Carroll, Wolfe, Tolkien, Le Guin
- Spinoza, Hume, Wittgenstein, Russell

**Science & Medicine:**
- Darwin, Feynman, Sagan-era figures, Wright (evolution), Shulgin (pharmacology)

**AI Safety & Rationality:**
- I. J. Good, Vernor Vinge, Aaron Swartz, Daniel Kahneman

The list reflects Gwern's intellectual influences and the broader rationalist/tech community's heroes.

## Special Features

### Timezone Consistency

Using `TZ="America/New_York"` ensures the memorial date changes at midnight Eastern Time, regardless of server timezone configuration. This provides consistency if the server is located elsewhere or during daylight saving transitions.

### Zero Runtime Cost

By pre-generating the nginx config via cron instead of executing a script per request, the system adds zero latency to page loads. The header is hardcoded into the nginx config at reload time.

### Graceful Empty Days

On days without memorials, the script outputs nothing, resulting in an empty include file. Nginx simply skips the include, and no header is set. This is cleaner than setting a placeholder or always showing "Terry Pratchett."

### Random Rotation for Shared Dates

The random selection for multi-person dates ensures long-term fairness when a comma-separated list is used. Duplicate keys do not accumulate; the last entry wins.

### Leap Year Handling

February 29 includes Robert H. Brower (died 2004-02-29), ensuring leap year memorials work correctly.

## Extending the Memorial List

To add a new memorial:

1. Find the person's death date (MM-DD format)
2. Add to the `memorials` array:
   ```bash
   ["MM-DD"]="Name"
   ```
3. If the date already exists, append with comma-separator:
   ```bash
   ["01-14"]="Kurt Gödel, Lewis Carroll, New Person"
   ```
4. Test with:
   ```bash
   TZ="America/New_York" bash memoriam.sh
   ```

The validation logic will catch duplicate entries or formatting errors.

## Historical Context

References in script comments:
- [GNU Terry Pratchett website](http://www.gnuterrypratchett.com/)
- [xclacksoverhead.org](http://xclacksoverhead.org/home/about)
- [Reddit discussion](https://www.reddit.com/r/discworld/comments/2yt9j6/gnu_terry_pratchett/)
- [Guardian coverage](https://www.theguardian.com/books/shortcuts/2015/mar/17/terry-pratchetts-name-lives-on-in-the-clacks-with-hidden-web-code)
- [Hacker News discussion](https://news.ycombinator.com/item?id=31348217)

The tradition became popular enough that Mozilla Firefox and other projects briefly considered adding native browser support for detecting and displaying the header.

---

## See Also

- [gwern.net.conf](/nginx/gwern-net-conf) - Main nginx config that includes the generated memorial header
- [twdne.conf](/nginx/twdne-conf) - Another nginx config (memorial header not included here)
- [rsyncd.conf](/nginx/rsyncd-conf) - Rsync daemon config (separate service)
- [sync.sh](/backend/sync-sh) - Build orchestrator (may trigger memorial regeneration)
- [default.html](/templates/default) - Page template served with memorial headers

## Example Header Values by Month

- **January:** Niklaus Wirth, Derek Parfit, Richard Hamming, Kurt Gödel/Lewis Carroll, Aaron Swartz
- **February:** Bertrand Russell, John von Neumann, Herbert Simon, Richard Feynman, Claude Shannon/Donald Keene, Freeman Dyson
- **April:** Isaac Asimov, Thomas Bayes, Francis Bacon, John Conway, Gene Wolfe, Charles Darwin/Daniel Dennett, **Terry Pratchett**, Ludwig Wittgenstein, E. T. Jaynes
- **June:** Alan Turing, Jorge Luis Borges/G.K. Chesterton
- **September:** J.R.R. Tolkien, Norman Borlaug
- **December:** Ray Solomonoff, Charles Babbage, Ian Murdock
