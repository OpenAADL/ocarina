"""Implementation of a Pygments Lexer for the AADL language."""

"""
* Copyright (c) 2018 Contributors
*
* This program and the accompanying materials are made
* available under the terms of the Eclipse Public License 2.0
* which is available at https://www.eclipse.org/legal/epl-2.0/
*
* SPDX-License-Identifier: EPL-2.0
"""

"""
Original implementation: Sam Procter
Updates: Jerome Hugues, Alexey Khoroshilov
"""

import sys
import re

from pygments.lexer import RegexLexer, include, bygroups
from pygments.token import Error, Punctuation, Literal, Token, \
     Text, Comment, Operator, Keyword, Name, String, Number, Generic, \
     Whitespace

__all__ = ['AADLLexer']

class AADLLexer(RegexLexer):
    """
    Pygments parser for AADL models. See <http://www.aadl.info> for
    more details.
    """

    name = 'AADL'
    aliases = ['aadl']
    filenames = ['*.aadl']
    mimetypes = ['text/x-aadl']

    flags = re.MULTILINE | re.DOTALL | re.IGNORECASE

    iden_rex = r'[a-zA-Z_][a-zA-Z0-9_\.]*'
    class_iden_rex = r'(' + iden_rex + r')(::)('+ iden_rex + r')'
    definition_rex = r'(' + iden_rex + r')' +  r'(\s*:\s*)\b'
    component_category = r'(abstract|data|subprogram|subprogram\s+group|thread|thread\s+group|process|memory|processor|bus|device|virtual\s+processor|virtual\s+bus|system)\b'

    with_tuple = (r'(with)(\s+)', bygroups(Keyword, Whitespace), 'with-list')
    text_tuple = (r'([^\S\n]+)', Text)
    terminator_tuple = (r'(;)(\s*)', bygroups(Punctuation, Whitespace), '#pop')
    comment_tuple = (r'(--[^\n]*\n)', Comment.Single)
    comment_whitespace_tuple = (r'(--[^\n]*\n)(\s+)', bygroups(Comment.Single, Whitespace))
    accesses_tuple = (r'(bus|subprogram|subprogram\s+group|data)(\s+)(access)\b', bygroups(Keyword, Whitespace, Keyword))
    features_tuple = (r'(feature|port|event\s+port|data\s+port|event\s+data\s+port|feature\s+group)\b', Keyword)

    tokens = {
        'packageOrSystem': [
             text_tuple,
             (r'(implementation)(\s+)(' + iden_rex + r')', bygroups(Name.Class, Whitespace, Name.Class), '#pop'),
             (iden_rex, Name.Class, '#pop'),
         ],
        'annex': [
             (r'(\s*)(' + iden_rex + r')(\s*)({\*\*.*\*\*})(\s*)(;)',
                     bygroups(Whitespace, Name.Class, Whitespace, Comment.Multiline, Whitespace, Punctuation)),
         ],
        'with-list' : [
            (r'\s*(,)\s*', Punctuation),
            (r'[a-zA-Z_]\w*', Name.Namespace),
            terminator_tuple,
        ],
        'alias-body' : [
            (component_category, Keyword.Declaration),
            (r'(\s+)', Whitespace),
            (class_iden_rex, bygroups(Name.Class, Punctuation, Name.Entity)),
            terminator_tuple,
        ],
        'package-declaration' : [
            text_tuple,
            (r'(implementation)', Keyword.Declaration),
            (r'(' + iden_rex + r')(;)', bygroups(Name.Class, Punctuation), '#pop'),
            (class_iden_rex + r'(;)', bygroups(Name.Class, Punctuation, Name.Entity, Punctuation), '#pop'),
            (r'(' + iden_rex + r')(\s*)(extends)(\s*)', bygroups(Name.Class, Whitespace, Keyword.Declaration, Whitespace)),
            (class_iden_rex, bygroups(Name.Class, Punctuation, Name.Entity), '#pop'),
            (iden_rex, Name.Class, '#pop'),
        ],
        'declaration' : [
            text_tuple,
            (r'(in|out|event|data)', Keyword),
            (r'(provides|requires)', Keyword),
            features_tuple,
            accesses_tuple,
            (r'(flow|path|thread|subprogram)', Keyword),
            (component_category, Keyword),
            (class_iden_rex, bygroups(Name.Class, Punctuation, Name)),
            (r'(' + iden_rex + r')(\s*)(->|<-|<->)(\s*)('+ iden_rex + r')', bygroups(Name, Whitespace, Operator, Whitespace, Name.Variable)),
            (iden_rex, Name.Function),
            (r'({)(\s+)', bygroups(Punctuation, Whitespace), 'property-constant-declaration'),
            (r'}', Punctuation),
            terminator_tuple,
        ],
        'applies-to' : [
            text_tuple,
            (r'\(', Punctuation),
            (r'\s*(,)\s*', Punctuation),
            (r'\s*(\*\*)\s*', Operator),
            features_tuple,
            accesses_tuple,
            (component_category, Keyword),
            (class_iden_rex, bygroups(Name.Class, Punctuation, Name.Entity)),
            (r'(' + iden_rex + r')', Name.Class),
            (r'(\{)(' + iden_rex + r')(\})', bygroups(Punctuation, Name.Class, Punctuation)),
            (r'\)', Punctuation),
            (r';', Punctuation, '#pop:2'),
        ],
        'property-value' : [
            (r'(true|false)', Keyword.Constant),
            (r'\(', Punctuation),
            (r'\)', Punctuation),
            (r',', Punctuation),
            (r'[0-9]+\.[0-9]*', Number.Float),
            (r'[0-9]+', Number.Integer),
            (r'(reference)(\s*)(\()(' + iden_rex + ')(\))',
             bygroups(Keyword.Declaration, Whitespace, Punctuation, Name.Variable.Instance, Punctuation)),
            (r'"[^"]*"', Literal.String.Double),
            (r'(\s*)(\.\.)(\s+)', bygroups(Whitespace, Operator, Whitespace)),
            (class_iden_rex, bygroups(Name.Class, Punctuation, Name.Variable)),
            (r'(\s*)(applies)(\s+)(to)(\s+)', bygroups(Whitespace, Keyword.Declaration, Whitespace, Keyword.Declaration, Whitespace), 'applies-to'),
            (r'(' + iden_rex +r')', Name.Constant),
            (r'(\[)(\s*)', bygroups(Punctuation, Whitespace), 'record_term'),
            (r'(\s+)', Whitespace),
            terminator_tuple,
        ],
        'record_term' : [
            (r'(' + iden_rex + r')(\s*)(=>)(\s*)', bygroups(Name.Class, Whitespace, Operator, Whitespace), 'property-value'),
            (r'(\])', Punctuation, '#pop'),
        ],
        'property-section-property-value' : [
            include('property-value'),
            terminator_tuple,
        ],
        'property-constant-value' : [
            include('property-value'),
            (r'(;)(\s+)', bygroups(Punctuation, Whitespace), '#pop:2')
        ],
        'aggregate-property-constant-list' : [
            (r'(' + iden_rex + r')(\s*)(=>)(\s*)', bygroups(Name.Class, Whitespace, Operator, Whitespace)),
            (r'\s*;\s*', Punctuation),
            include('property-value'),
            (r'(\]\s*;)(\s+)', bygroups(Punctuation, Whitespace), '#pop:2'),
        ],
        'property-declaration' : [
            comment_tuple,
            (r'(inherit|list\s+of)', Keyword.Declaration),
            # aadl property types
            (r'(aadlboolean|aadlinteger|aadlreal|aadlstring|enumeration|range\s+of|classifier|reference|record)', Keyword.Type),
            (r'(,|\(|\)|\+|-|\.\.|:|;)', Punctuation),
            (r'(units)(\s*)(\()', bygroups(Keyword.Declaration, Whitespace, Punctuation), 'units-list'),
            (r'[0-9]+', Number.Integer),
            features_tuple,
            accesses_tuple,
            (component_category, Keyword.Type),
            (r'(=>)(\s*)', bygroups(Operator, Whitespace), 'applies-to-property-value'),
            (r'(applies)(\s+)(to)(\s+)', bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration, Whitespace), 'applies-to'),
            (class_iden_rex, Name.Class),
            (r'(' + iden_rex + r')', Name.Class),
            (r'(\s+)', Whitespace),
        ],
        'units-list' : [
            comment_tuple,
            (r'(' + iden_rex + r')', Name.Class),
            (r'(,|\*|=>)', Punctuation),
            (r'(\s+)', Whitespace),
            (r'(\))', Punctuation, '#pop'),
        ],
        'applies-to-property-value' : [
            (r'(applies)(\s+)(to)(\s+)', bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration, Whitespace), 'applies-to'),
            include('property-value'),
        ],
        'property-constant-declaration' : [
            text_tuple,
            (class_iden_rex + r'(\s*)(=>)(\s*)(\[)(\s*)', bygroups(Name.Class, Punctuation, Name.Constant, Whitespace, Operator, Whitespace, Punctuation, Whitespace), 'aggregate-property-constant-list'),
            (r'(' + iden_rex + r')(\s*)(=>)(\s*)(\[)(\s*)', bygroups(Name.Class, Whitespace, Operator, Whitespace, Punctuation, Whitespace), 'aggregate-property-constant-list'),
            (class_iden_rex + r'(\s*)(=>)(\s*)', bygroups(Name.Class, Punctuation, Name.Constant, Whitespace, Operator, Whitespace), 'property-constant-value'),
            (r'(' + iden_rex + r')(\s*)(=>)(\s*)', bygroups(Name.Class, Whitespace, Operator, Whitespace), 'property-constant-value'),
        ],
        'property-set' : [
            comment_tuple,
            with_tuple,
            (r'(' + iden_rex + r')(\s+)(is)(\s+)', bygroups(Name.Class, Whitespace, Keyword.Namespace, Whitespace)),
            (definition_rex + r'(constant)', bygroups(Name.Variable.Global, Punctuation, Keyword), 'property-constant-declaration'),
            (definition_rex, bygroups(Name.Variable.Global, Punctuation), 'property-declaration'),
            (r'(end)(\s+)(' + iden_rex + r')(;)', bygroups(Keyword.Namespace, Whitespace, Name.Class, Punctuation), '#pop'),
            (r'(\s+)', Whitespace),
        ],
        'property-section' : [
            text_tuple,
            comment_whitespace_tuple,
            (class_iden_rex + r'(\s*)(=>)(\s*)',
             bygroups(Name.Class, Punctuation, Name.Entity, Whitespace, Operator, Whitespace), 'property-section-property-value'),
            (r'(' + iden_rex + r')(\s*)(=>)(\s*)',
             bygroups(Name.Class, Whitespace, Operator, Whitespace), 'property-section-property-value'),
            (r'(\*\*})(\s*)(;)', bygroups(Punctuation, Whitespace, Punctuation), '#pop'),
            (r'([\s+])', Whitespace),
            (r'', Whitespace, '#pop'),
        ],
        'call-section' : [
            text_tuple,
            comment_whitespace_tuple,
            (r'(' + iden_rex + r')(\s*)(:)(\s*)({)(\s*)', bygroups(Name.Class, Whitespace, Punctuation, Whitespace, Punctuation, Whitespace)),
            (definition_rex, bygroups(Name.Variable, Punctuation), 'declaration'),
            (r'}', Punctuation),
            terminator_tuple,
        ],
        'id-or-classid': [
            (class_iden_rex, bygroups(Name.Class, Punctuation, Name.Entity), '#pop'),
            (r'(' + iden_rex + r')', Name.Entity, '#pop'),
        ],
        'semicolon': [
            (r'(\s*)(;)', bygroups(Whitespace, Punctuation), '#pop'),
        ],
        'emv2-annex': [
            (r'(use)(\s+)(types|type\s+equivalence|mappings|behavior)(\s+)',
                    bygroups(Keyword.Namespace, Whitespace, Keyword.Namespace, Whitespace), ('semicolon', 'id-or-classid')),
            (r'(error\s+propagations)(\s+)', bygroups(Keyword.Namespace, Whitespace), 'emv2-propagations'),
            (r'(component\s+error\s+behavior)(\s+)', bygroups(Keyword.Namespace, Whitespace), 'emv2-component'),
            (r'(\*\*})(\s*)(;)', bygroups(Punctuation, Whitespace, Punctuation), '#pop'),
            (r'(\s+)', Whitespace),
         ],
        'emv2-propagations': [
            (r'(not|in|out|propagation)', Keyword.Namespace),
            (r'(:|{|\*|::|}|;)', Punctuation),
            (r'(end\s+propagations)(\s*)(;)', bygroups(Keyword.Namespace, Whitespace, Punctuation), '#pop'),
            (r'(' + iden_rex + r')', Name.Entity),
            (r'(\s+)', Whitespace),
        ],
        'emv2-component': [
            (r'(use)(\s+)(transformations)(\s+)',
                    bygroups(Keyword.Namespace, Whitespace, Keyword.Namespace, Whitespace), ('semicolon', 'id-or-classid')),
            (r'(events|transitions|propagations|detections|mode\s+mappings)', Keyword.Namespace),
            (r'(all|noerror)', Keyword.Constant),
            (r'(:|;|{|}|\(|\))', Punctuation),
            (r'(-\[)(\s*)', bygroups(Punctuation, Whitespace), 'emv2-error-condition'),
            (r'(end\s+component)(\s*)(;)', bygroups(Keyword.Namespace, Whitespace, Punctuation), '#pop'),
            (r'(' + iden_rex + r')', Name.Entity),
            (r'(\s+)', Whitespace),
        ],
        'emv2-error-condition': [
            (r'(and|ormore|orless|or)', Keyword.Constant),
            (r'(\(|\)|\{|\}|::)', Punctuation),
            (r'[0-9]+', Number.Integer),
            (r'(\]->)', Punctuation, '#pop'),
            (r'(' + iden_rex + r')', Name.Entity),
            (r'(\s+)', Whitespace),
        ],
        'root': [
            (r'(\n\s*|\t)', Whitespace),
            comment_tuple,
            (r'(package)(\s+)', bygroups(Keyword.Namespace, Text), 'packageOrSystem'),
            (r'(public|private)', Keyword.Namespace),
            # import_declaration
            with_tuple,
            # alias_declaration
            (r'(' + iden_rex + r')(\s+)(renames)(\s+)', bygroups(Name.Namespace, Whitespace, Keyword, Whitespace), 'alias-body'),
            (r'(annex)(\s+)(EMV2)(\s*)({\*\*)', bygroups(Keyword.Namespace, Whitespace, Name.Namespace, Whitespace, Punctuation),
                                             'emv2-annex'),
            (r'(annex)(\s+)',  bygroups(Keyword.Namespace, Whitespace), 'annex'),
            (component_category + r'(\s+)', bygroups(Keyword.Type, Whitespace), 'package-declaration'),
            (r'(calls)(\s+)',bygroups(Keyword.Namespace, Whitespace),'call-section'),
            (r'(subcomponents|connections|features|flows)(\s+)', bygroups(Keyword.Namespace, Whitespace)),
            (definition_rex, bygroups(Name.Variable, Punctuation), 'declaration'),
            (r'(properties)(\s*)', bygroups(Keyword.Namespace, Whitespace), 'property-section'),
            (r'(end)(\s+)', bygroups(Keyword.Namespace, Whitespace), 'package-declaration'),
            (r'(property\s+set)(\s+)', bygroups(Keyword.Namespace, Whitespace), 'property-set'),
            (r'(\s+)', Whitespace),
        ]
    }

class AADLPropertyLexer(AADLLexer):
    """
    Pygments parser for AADL property declarations.
    """

    name = 'AADL Property'
    aliases = ['aadlproperty']
    filenames = []
    mimetypes = []

    flags = re.MULTILINE | re.DOTALL | re.IGNORECASE
    tokens = AADLLexer.tokens.copy()
    tokens['root'] = [
            (r'(' + AADLLexer.iden_rex + r')(\s*)(:)(\s*)',
                     bygroups(Name.Variable, Whitespace, Punctuation, Whitespace), 'property-declaration'),
                     ]

class AADLIdLexer(AADLLexer):
    """
    Pygments parser for AADL identifiers.
    """

    name = 'AADL Id'
    aliases = ['aadlid']
    filenames = []
    mimetypes = []

    flags = re.MULTILINE | re.DOTALL | re.IGNORECASE
    tokens = AADLLexer.tokens.copy()
    tokens['root'] = [
            AADLLexer.features_tuple,
            AADLLexer.accesses_tuple,
            (AADLLexer.component_category, Keyword),
            (r'[0-9]+', Number.Integer),
            (r'(' + AADLLexer.iden_rex + r')', Name.Variable),
            (r'(::|,)', Punctuation),
            (r'(\s+)', Whitespace),
                     ]
