import email
import sys

if __name__=='__main__':
    if len(sys.argv)<2:
        print "Please enter a file to extract attachments from"
        sys.exit(1)

    msg = email.message_from_file(open(sys.argv[1]))
    for pl in msg.get_payload():
        if pl.get_filename(): # if it is an attachment
            open(pl.get_filename(), 'wb').write(pl.get_payload(decode=True))
