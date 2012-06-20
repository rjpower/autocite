/**
 * A Hadoop record reader for reading Warc Records
 *
 * (C) 2009 - Carnegie Mellon University
 *
 * 1. Redistributions of this source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. The names "Lemur", "Indri", "University of Massachusetts",
 *    "Carnegie Mellon", and "lemurproject" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. To obtain permission, contact
 *    license@lemurproject.org.
 *
 * 4. Products derived from this software may not be called "Lemur" or "Indri"
 *    nor may "Lemur" or "Indri" appear in their names without prior written
 *    permission of The Lemur Project. To obtain permission,
 *    contact license@lemurproject.org.
 *
 * THIS SOFTWARE IS PROVIDED BY THE LEMUR PROJECT AS PART OF THE CLUEWEB09
 * PROJECT AND OTHER CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 * NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * @author mhoy@cs.cmu.edu (Mark J. Hoy)
 */

package edu.cmu.lemurproject;

import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.mapred.FileSplit;
import org.apache.hadoop.mapred.RecordReader;
import org.archive.io.ArchiveRecord;
import org.archive.io.warc.WARCReaderFactory;

public class WarcFileRecordReader implements
		RecordReader<LongWritable, WritableArchiveRecord> {
	public static final Log LOG = LogFactory.getLog(WarcFileRecordReader.class);

	private long totalFileSize = 0;
	private long totalNumBytesRead = 0;
	private Path path;
	private InputStream stream;
	private java.util.Iterator<ArchiveRecord> iterator;

	public WarcFileRecordReader(Configuration conf, FileSplit split)
			throws IOException {
		path = split.getPath();
		FileSystem fs = path.getFileSystem(conf);
		totalFileSize = fs.getFileStatus(path).getLen();
		stream = fs.open(path);
		iterator = WARCReaderFactory.get(path.getName(), stream, true)
				.iterator();
	}

	public boolean next(LongWritable key, WritableArchiveRecord value)
			throws IOException {
		if (!iterator.hasNext()) {
			return false;
		}

		key.set(0);
		value.data = iterator.next();
		return true;
	}

	public LongWritable createKey() {
		return new LongWritable();
	}

	public WritableArchiveRecord createValue() {
		return new WritableArchiveRecord();
	}

	public long getPos() throws IOException {
		return totalNumBytesRead;
	}

	public void close() throws IOException {
		totalNumBytesRead = totalFileSize;
	}

	public float getProgress() throws IOException {
		return (float) totalNumBytesRead / (float) totalFileSize;
	}

}
