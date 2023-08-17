import sys
import argparse
import google.auth
from google.cloud import datastore


def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--project_sb', help='PROJECT ID.', required=True)
    args = vars(parser.parse_args())
    project_sb = args['project_sb']
    
    # get default creds
    try:
        credentials, _ = google.auth.default()
        ds = datastore.Client(credentials=credentials, project = project_sb)
    except Exception as e:
        print("ERROR :: ", e)
        sys.exit(e)
    else:
      # query the Datastore
      query = ds.query(kind='SandboxConfig').fetch()
      sb_list = list(query)    
      lst = [','.join([sb['SandboxName'], sb['ProjectID']]) for sb in sb_list]
    
      # print to the screen
      print(';'.join(lst))


if __name__ == '__main__':
    main()
